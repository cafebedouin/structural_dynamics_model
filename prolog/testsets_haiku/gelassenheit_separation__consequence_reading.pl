% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__consequence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__consequence_reading, []).

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
 *   constraint_id: gelassenheit_separation__consequence_reading
 *   human_readable: Gelassenheit Separation via Community Practice Consequence
 *   domain: religious/social/technological
 *
 * SUMMARY:
 *   This constraint is the consequence-reading of the contested
 *   gelassenheit_separation kernel: Separation from worldly systems is
 *   interpreted as the preservation of community practices (visiting, mutual
 *   aid, geographic rootedness) rather than as avoidance of structural
 *   entanglement or rejection of worldly appearance. Under this reading, a
 *   telephone in a barn is acceptable if it preserves rootedness (the person
 *   remains tied to place); a telephone in the home is forbidden because it
 *   erodes visiting networks (alternatives to co-presence multiply). A
 *   tractor is acceptable for belt power only (functionally isolated, work
 *   still done collectively) but not for personal transport (competition with
 *   walking/visiting). The authority structure (church leadership) makes
 *   fine-grained contextual judgments about technology's consequence for
 *   community practice. This reading coexists with two sibling readings: the
 *   artifact_reading (technology is forbidden if visibly worldly, regardless
 *   of function) and the principle_reading (technology is acceptable if
 *   functionally isolated, regardless of consequence). All three readings
 *   operate in different communities; this story generates ONLY the
 *   consequence reading.
 *
 * KEY AGENTS:
 *   - Church leadership: institutional agenda-setter; interprets doctrine, enforces boundaries via membership discipline
 *   - Multigenerational households: moderate-power beneficiaries/payers; benefit from preserved visiting/mutual aid; pay via technology restriction; identity-locked
 *   - Young adults in transition: powerless payers; bear primary cost of ambiguous boundaries; identity-locked; rumspringa testing carries social risk
 *   - Competing interpretations (artifact & principle readings): excluded from this authority structure; hold alternate doctrine in their own communities
 *   - Visiting networks & mutual aid economy: non-agent beneficiaries; narrative placeholders for the practices the constraint preserves
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__consequence_reading, 0.28).
domain_priors:suppression_score(gelassenheit_separation__consequence_reading, 0.22).
domain_priors:theater_ratio(gelassenheit_separation__consequence_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__consequence_reading, tangled_rope).
narrative_ontology:human_readable(gelassenheit_separation__consequence_reading, "Gelassenheit Separation via Community Practice Consequence").
narrative_ontology:topic_domain(gelassenheit_separation__consequence_reading, "religious/social/technological").

domain_priors:requires_active_enforcement(gelassenheit_separation__consequence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__consequence_reading, '6bc9d238-461c-45aa-bb0e-7169db3001f9').
narrative_ontology:cs_kernel_codification('6bc9d238-461c-45aa-bb0e-7169db3001f9', distributed).
narrative_ontology:cs_authority_grounding('6bc9d238-461c-45aa-bb0e-7169db3001f9', lineage).
narrative_ontology:cs_interpretation_layer_present('6bc9d238-461c-45aa-bb0e-7169db3001f9').
narrative_ontology:cs_reading_relation('6bc9d238-461c-45aa-bb0e-7169db3001f9', gelassenheit_separation__artifact_reading, coexists_with).
narrative_ontology:cs_reading_relation('6bc9d238-461c-45aa-bb0e-7169db3001f9', gelassenheit_separation__principle_reading, coexists_with).
narrative_ontology:cs_axiom('6bc9d238-461c-45aa-bb0e-7169db3001f9', foundational, separation_defined_by_community_practice_consequence).
narrative_ontology:cs_axiom_status(separation_defined_by_community_practice_consequence, holdable).
narrative_ontology:cs_axiom_grounding('6bc9d238-461c-45aa-bb0e-7169db3001f9', separation_defined_by_community_practice_consequence, instrumental).
narrative_ontology:cs_axiom('6bc9d238-461c-45aa-bb0e-7169db3001f9', secondary, authority_judges_consequence_via_contextual_interpretation).
narrative_ontology:cs_axiom_status(authority_judges_consequence_via_contextual_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('6bc9d238-461c-45aa-bb0e-7169db3001f9', authority_judges_consequence_via_contextual_interpretation, conventional).
narrative_ontology:cs_reference_frame('6bc9d238-461c-45aa-bb0e-7169db3001f9', visiting_and_mutual_aid_centered_community_economy).
narrative_ontology:cs_drift_state('6bc9d238-461c-45aa-bb0e-7169db3001f9', contemporary_wage_work_dominance, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6bc9d238-461c-45aa-bb0e-7169db3001f9', '').
narrative_ontology:cs_kernel_id(gelassenheit_separation__consequence_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, multigenerational_community_continuity).
narrative_ontology:constraint_victim(gelassenheit_separation__consequence_reading, individual_convenience_seekers).
narrative_ontology:constraint_victim(gelassenheit_separation__consequence_reading, boundary_ambiguity_bearers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, multigenerational_households).
narrative_ontology:constraint_victim(gelassenheit_separation__consequence_reading, multigenerational_households).
narrative_ontology:constraint_victim(gelassenheit_separation__consequence_reading, young_adults_in_transition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Gelassenheit separation doctrine; decides which technologies preserve or erode community practice (visiting, mutual aid, geographic rootedness). Enforces via membership review and shunning risk. Makes contextual judgments: telephone in barn is acceptable (rootedness preserved), telephone in home is forbidden (visiting erodes). Carries legitimacy from lineage (historical Anabaptist teaching) and community consent. Authority depends on maintaining the boundary's integrity and coherence.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, church_leadership, agenda_setter,
    institutional, generational, constrained, regional).

% Benefit from the constraint because it protects conditions for visiting networks and mutual aid — the economic fabric that makes multigenerational co-residence viable. Pay by accepting technology restrictions that convenience-seeking would otherwise permit. Identity fused with community membership; exit means leaving the entire kinship/economic ecosystem. Visits to neighbors, barn work, harvest coordination, child-minding shared across households are structurally enabled by the constraint's discipline.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, multigenerational_households, beneficiary,
    moderate, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(gelassenheit_separation__consequence_reading, multigenerational_households, payer).

% Bear the primary cost of ambiguous boundaries: whether a smartphone is forbidden depends on context judgments made by authority. Rumspringa (choice period) nominally permits exploration, but ambiguity about what is permitted-for-work vs. forbidden-at-home creates compliance friction and social surveillance. Their choices depend on authority interpretation, not on clear rules; they carry identity risk for boundary testing. Most remain identity-locked; those who exit leave the community entirely.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, young_adults_in_transition, payer,
    powerless, biographical, identity_locked, local).

% The artifact_reading (technology forbidden if visibly worldly) and principle_reading (technology acceptable if functionally isolated) represent alternative doctrine interpretations held by other communities/traditions. Under this consequence_reading, they are partially excluded from the authority structure that interprets Gelassenheit. They would argue different technologies should be permitted (phones everywhere if functionally isolated, or phones nowhere if visibility is the test). Their exclusion is structural — one community cannot adopt two incompatible readings simultaneously.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, competing_interpretations, excluded,
    moderate, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gelassenheit_separation__consequence_reading, multigenerational_households).
narrative_ontology:fixing_cost_class(gelassenheit_separation__consequence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the material and social conditions for face-to-face visiting, multigenerational household co-residence, and informal mutual aid — the economic ties that bind the community. Technology evaluation focuses on whether adoption erodes these practices, not on the technology's appearance or internal functioning.
% TRANSFER_FUNCTION: Restricts access to convenience technologies in domains (homes, personal communication) where adoption would compress visiting time or weaken reliance on proximity-based mutual aid. The cost is individual time-savings forgone; the benefit is community-level economic resilience and visiting frequency.
% ABSENT_VOICES: Competing doctrine readers (artifact and principle interpretations) are structurally excluded from authoring the rules for this community; they hold authority only in their own communities. Individual convenience-seekers who would argue for unrestricted technology access also lack standing in the authority structure.
% DISAPPEARANCE_RATIONALE: If the consequence-based separation constraint and its enforcement vanished, visiting frequency would decline over one generation as convenient substitutes (phone calls, text) displaced co-presence. Multigenerational co-residence patterns would shift as shared work declined. The informal mutual aid economy would contract and be replaced by monetary transactions or external services. The community's economic resilience and intergenerational knowledge transfer would measurably degrade.
% FOUNDING_PROBLEM: Early industrial-era technology adoption threatened to fragment Anabaptist communities by enabling individuals to defer mutual dependence (machinery replacing shared work, wage labor replacing household economy, town-based commerce replacing local barter). The founding problem was: how do we preserve the conditions that make community membership economically intelligible?
% FOUNDING_PROBLEM_CORROBORATION: Church leadership attests the founding problem remains live and that continued restriction on consequence-eroding technologies is necessary. Sociologists studying Amish/Mennonite economic outcomes attest that visiting frequency and mutual-aid structures correlate with technology restrictions and are substantially eroded where restrictions are lifted. Competing-reading communities attest the founding problem is partially obsolete (modern employment markets are now the driver) and technology restriction should now focus on systemic entanglement (principle) or cultural visibility (artifact) rather than consequence. Youth in rumspringa attest the problem remains lived — they directly experience the friction between convenience and community obligations.
narrative_ontology:disappearance_verdict(gelassenheit_separation__consequence_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__consequence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__consequence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gelassenheit_separation__consequence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__consequence_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__consequence_reading_tests).
:- end_tests(gelassenheit_separation__consequence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is LOW (0.28 terminal) because the constraint's primary function is genuine community practice preservation, not the extraction of rents. Suppression is also LOW (0.22) because the constraint operates through identity-loyalty and peer consensus rather than through coercive force — young adults who stay in community do so because the constraint makes sense to them, not because they are forced. Theater is MINIMAL (0.18) because the rule-making is explicitly consequence-focused and openly discussed; there is little performative cover-story. However, extractiveness is NOT ZERO (as it would be for a pure rope) because the younger generation bears real cost (convenience forgone, boundary ambiguity creating surveillance risk) while multigenerational households — who benefit most — carry lower individual exit cost. The asymmetry is mild (hence tangled_rope, not snare) because beneficiaries and payers largely overlap: the same households that benefit from preserved mutual aid also pay via restriction; the young who pay heavily have the option (identity_locked exit via rumspringa and potential emigration) even if costly. The measurement series shows extractiveness rising early (t=0 to t=30) as successive cohorts test boundaries and authority responds with enforcement infrastructure, then stabilizing (t=30–50) as norms harden. Theater rises as enforcement becomes routine (ceremonial shunning, collective boundary-talk) but stays low because the underlying coordination function remains genuinely functional.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (young adults, convenience-seekers) experience this constraint as suppressive ambiguity: what exactly counts as consequence-eroding is decided by authority with a lag, so testing boundaries carries social risk. The beneficiary seat (church leadership, multigenerational households) experiences the same constraint as protective coordination: the fine-grained rules preserve the material conditions for community life. The dilemma is that the SAME technological decision (home phone? barn phone? tractor?) triggers the dilemma: authority must decide whether consequence is eroded. Young adults bear the judgment-delay cost; leadership bears the interpretation cost; multigenerational households benefit but are not the sole decision-makers. The engine computes this divergence from the power/exit/beneficiary structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Church leadership is agenda_setter with institutional power; their d is low (beneficiary end) because they set the rules and the constraint operates through their authority. Multigenerational households have moderate power and mixed directionality (d~0.45): they benefit from the coordination (visiting/mutual aid) but pay via restriction (no home phones). Young adults in transition are powerless and high-d (target end, ~0.75) because the ambiguous boundaries create compliance friction they uniquely bear. The two non-agent beneficiaries (visiting_networks, mutual_aid_economy) are analytical placeholders; they do not feed directionality. Competing interpretations are excluded (not payer or beneficiary, so they do not enter the directionality chain for this reading). The consequence_reading's directionality structure differs sharply from the sibling readings: under artifact_reading, technology visible-as-worldly is simply forbidden (no consequence evaluation), so directionality depends on who polices appearance (different authority allocation); under principle_reading, functionally isolated tech is permitted (directionality depends on who judges functional isolation). This reading's directionality is tied to consequence-judgment, which church leadership adjudicates.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint scores as tangled_rope (low-to-moderate extraction, genuine coordination function + asymmetric cost) rather than snare because: (1) the coordination problem is real — visiting and mutual aid do require technology discipline to remain central in household economy; (2) beneficiaries and payers substantially overlap; (3) the primary enforcement mechanism is identity-loyalty and peer consensus, not coercive punishment. It is NOT rope (pure coordination) because the cost asymmetry is real: young adults pay convenience-forgone without receiving the economic benefit (they have not yet built the multigenerational co-residence structure); they are locked by identity rather than choice. The theater ratio is kept deliberately low (0.18) because the rule-making is explicit and consequence-focused — there is no false story here, only genuine contestation about what consequences matter. The constraint resolves mandatrophy via the founding_problem_status=contested verdict: the founding problem is LIVE (community continuation does depend on visiting/mutual aid), but the MEANS debate is contestable (consequence vs. principle vs. artifact). This reading claims its resolution is the most defensible because it is consequence-centered and operates via authority interpretation of real effects, not via appearance-policing or abstract functional purity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consequence_vs_appearance_boundary,
    'Is the distinction between consequence-based and artifact-based technology evaluation stable and generalizable, or do they inevitably converge as communities apply them?',
    'Empirical comparison across Anabaptist communities: track which technologies each reading permits/forbids and whether they produce systematically different outcomes. Long-term: do communities drift toward uniform restrictions despite differing readings?',
    'If the distinction is unstable (consequence and appearance evaluations converge), the three readings are functionally equivalent under this constraint and foreclosure may be more appropriate than coexistence. If stable, the readings remain genuinely alternative and coexistence is the accurate classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consequence_vs_appearance_boundary, empirical, 'Whether consequence and artifact readings produce empirically distinct technology policies over time.').

omega_variable(
    identity_lock_stability_under_boundary_ambiguity,
    'Does the ambiguity inherent to consequence-based rules (what counts as consequence-eroding?) stabilize identity-lock for young adults, or does it destabilize it by creating repeated compliance friction?',
    'Biographical cohort studies: track rumspringa outcomes and post-decision attachment (do young adults who endured boundary-ambiguity stress remain committed post-choice?). Compare against artifact or principle readings where rules are clearer.',
    'If ambiguity STABILIZES attachment (testing clarifies belief), suppression is lower and the constraint remains tangled_rope. If ambiguity DESTABILIZES attachment (stress drives emigration), suppression must be coded higher and the constraint drifts toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_stability_under_boundary_ambiguity, empirical, 'Whether boundary-ambiguity in consequence-based rules strengthens or weakens identity-lock.').

omega_variable(
    external_economic_pressure_displacement,
    'As external job markets and wage work become dominant (displacing multigenerational household economy), does the consequence-based reading become obsolete faster than principle or artifact readings?',
    'Temporal measurement: track whether extractiveness and suppression_requirement rise sharply when exogenous employment pressures intensify. Compare against principle/artifact readings in parallel communities.',
    'If consequence-reading becomes obsolete faster, it may require re-reading or may transition to piton (performs theater without real coordination function). The founding_problem_status would shift from ''live'' to ''dead'' more rapidly than in principle/artifact versions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_economic_pressure_displacement, empirical, 'Whether external economic shifts make consequence-based separation unmoored from actual community practice faster than principle/artifact versions.').

omega_variable(
    authority_interpretation_capture,
    'Does church leadership''s role as consequence-judge create risk of authority capture, where rules are bent to favor particular households or reduce enforcement burden?',
    'Ethnographic observation: track consistency of boundary decisions across similar technological cases. Survey young adults on perceived fairness of rule application.',
    'If capture is high, extractiveness should be recoded upward and suppression should reflect arbitrary enforcement. If capture is low, the constraint remains authentically consequence-focused.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_interpretation_capture, empirical, 'Whether consequence-based authority interpretation is vulnerable to capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__consequence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gela_tr_t0, gelassenheit_separation__consequence_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(gela_tr_t10, gelassenheit_separation__consequence_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(gela_tr_t20, gelassenheit_separation__consequence_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(gela_tr_t30, gelassenheit_separation__consequence_reading, theater_ratio, 30, 0.17).
narrative_ontology:measurement(gela_tr_t40, gelassenheit_separation__consequence_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(gela_tr_t50, gelassenheit_separation__consequence_reading, theater_ratio, 50, 0.18).

% Extraction over time
narrative_ontology:measurement(gela_be_t0, gelassenheit_separation__consequence_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(gela_be_t10, gelassenheit_separation__consequence_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(gela_be_t20, gelassenheit_separation__consequence_reading, base_extractiveness, 20, 0.25).
narrative_ontology:measurement(gela_be_t30, gelassenheit_separation__consequence_reading, base_extractiveness, 30, 0.28).
narrative_ontology:measurement(gela_be_t40, gelassenheit_separation__consequence_reading, base_extractiveness, 40, 0.28).
narrative_ontology:measurement(gela_be_t50, gelassenheit_separation__consequence_reading, base_extractiveness, 50, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(gela_su_t0, gelassenheit_separation__consequence_reading, suppression_requirement, 0, 0.16).
narrative_ontology:measurement(gela_su_t10, gelassenheit_separation__consequence_reading, suppression_requirement, 10, 0.19).
narrative_ontology:measurement(gela_su_t20, gelassenheit_separation__consequence_reading, suppression_requirement, 20, 0.21).
narrative_ontology:measurement(gela_su_t30, gelassenheit_separation__consequence_reading, suppression_requirement, 30, 0.22).
narrative_ontology:measurement(gela_su_t40, gelassenheit_separation__consequence_reading, suppression_requirement, 40, 0.22).
narrative_ontology:measurement(gela_su_t50, gelassenheit_separation__consequence_reading, suppression_requirement, 50, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__consequence_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(gelassenheit_separation__consequence_reading, 0.12).
narrative_ontology:affects_constraint(gelassenheit_separation__consequence_reading, gelassenheit_separation__artifact_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__consequence_reading, gelassenheit_separation__principle_reading).

% DUAL FORMULATION NOTE:
% The gelassenheit_separation kernel decomposes into three structurally distinct constraints — three different ways of defining 'separation from worldly systems' that operate in different communities simultaneously. This story (consequence_reading) is one reading; the others are artifact_reading and principle_reading. All three share the kernel but instantiate different ε values and authority structures. The consequence-reading operates via fine-grained contextual judgment (barn phone OK, home phone forbidden; tractor for belt power only); artifact-reading operates via appearance-policing (no technology that looks English, regardless of function); principle-reading operates via functional-isolation criteria (technology acceptable if structurally independent). Each reading is a valid constraint in communities that adopt it. They coexist as alternative interpretations of the same kernel, not as sequential historical stages — all three are live in contemporary Anabaptist communities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gelassenheit_separation__consequence_reading, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
