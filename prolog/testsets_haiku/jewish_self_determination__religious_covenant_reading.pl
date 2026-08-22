% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__religious_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__religious_covenant_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: jewish_self_determination__religious_covenant_reading
 *   human_readable: Jewish Self-Determination Through Divine Covenant (Religious Reading)
 *   domain: political_philosophy/religious_nationalism/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the RELIGIOUS COVENANT READING of the
 *   contested kernel 'jewish_self_determination'. The reading claims that
 *   Jewish territorial claim to the land of Israel derives from a divine
 *   covenant recorded in Torah and interpreted through rabbinic tradition,
 *   making sovereignty in that territory a religious obligation independent
 *   of secular political frameworks or pragmatic negotiation. This reading
 *   operates within the authority structure of religious Jewish tradition and
 *   institutions; it is not merely a political position but a theological
 *   claim about obligation. The constraint is claimed as MOUNTAIN (divine
 *   command treated as immutable natural law within the religious framework)
 *   but OPERATIONALIZED as TANGLED ROPE (the religious authority claim
 *   becomes entangled with state power and settlement policy, creating
 *   asymmetric extraction: secular negotiators and Palestinian claimants bear
 *   costs from the foreclosure of compromise-frameworks, while the religious
 *   reading and settlement enterprise benefit from the constraint's
 *   institutional dominance). The ε value of 0.82 reflects not the authority
 *   of the theological claim within its own framework (where it would
 *   approach 1.0 as absolute command) but the extractive force it exerts in
 *   the contested, multi-framework landscape where participants operate under
 *   different authority registers.
 *
 * KEY AGENTS:
 *   - religious_zionist_movement: Institutional agenda-setter advancing the covenant reading as foundation for sovereignty. Organized power, identity-locked commitment to territorial control as religious obligation.
 *   - settlement_enterprise: Beneficiary organization expanding residential settlement under religious-obligation framing. Identity-locked commitment; receives legitimacy and resources justified by the religious reading.
 *   - secular_zionist_negotiators: Payer. Institutional seat constrained by the religious reading's dominance; reduced diplomatic flexibility, heightened international friction. Trapped between coalition maintenance and negotiation viability.
 *   - palestinian_claimants: Payer. Face asymmetric negotiating frameworks; the religious-obligation reading forecloses mutual-compromise logic that would be intelligible to secular frameworks. Structurally excluded from authority over interpretation.
 *   - diasporist_critics: Excluded. Argue Jewish security is viable outside territorial sovereignty; treated as theologically incoherent by the religious reading's proponents.
 *   - liberal_nationalist_advocates: Excluded. Ground Jewish self-determination in universal nationalism principles; marginalized where religious covenant reading dominates institutional settings.
 *   - international_secular_framework: Observer. Diplomatic and legal apparatus operating under territorial-law and secular-rights assumptions; observes a constraint that operates orthogonally to its epistemic foundations.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__religious_covenant_reading, 0.82).
domain_priors:suppression_score(jewish_self_determination__religious_covenant_reading, 0.71).
domain_priors:theater_ratio(jewish_self_determination__religious_covenant_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__religious_covenant_reading, mountain).
narrative_ontology:human_readable(jewish_self_determination__religious_covenant_reading, "Jewish Self-Determination Through Divine Covenant (Religious Reading)").
narrative_ontology:topic_domain(jewish_self_determination__religious_covenant_reading, "political_philosophy/religious_nationalism/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__religious_covenant_reading).
domain_priors:emerges_naturally(jewish_self_determination__religious_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__religious_covenant_reading, '211d1573-bbd6-4b55-9da2-cd0b0b4ca399').
narrative_ontology:cs_kernel_codification('211d1573-bbd6-4b55-9da2-cd0b0b4ca399', fixed_text).
narrative_ontology:cs_authority_grounding('211d1573-bbd6-4b55-9da2-cd0b0b4ca399', lineage).
narrative_ontology:cs_interpretation_layer_present('211d1573-bbd6-4b55-9da2-cd0b0b4ca399').
narrative_ontology:cs_reading_relation('211d1573-bbd6-4b55-9da2-cd0b0b4ca399', jewish_self_determination__liberal_nationalist_reading, forecloses).
narrative_ontology:cs_reading_relation('211d1573-bbd6-4b55-9da2-cd0b0b4ca399', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('211d1573-bbd6-4b55-9da2-cd0b0b4ca399', jewish_self_determination__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('211d1573-bbd6-4b55-9da2-cd0b0b4ca399', jewish_self_determination__diasporist_reading, forecloses).
narrative_ontology:cs_axiom('211d1573-bbd6-4b55-9da2-cd0b0b4ca399', foundational, divine_covenant_territorial_obligation).
narrative_ontology:cs_axiom_status(divine_covenant_territorial_obligation, holdable).
narrative_ontology:cs_axiom_grounding('211d1573-bbd6-4b55-9da2-cd0b0b4ca399', divine_covenant_territorial_obligation, deontological).
narrative_ontology:cs_axiom('211d1573-bbd6-4b55-9da2-cd0b0b4ca399', foundational, religious_authority_precedence_over_secular).
narrative_ontology:cs_axiom_status(religious_authority_precedence_over_secular, holdable).
narrative_ontology:cs_axiom_grounding('211d1573-bbd6-4b55-9da2-cd0b0b4ca399', religious_authority_precedence_over_secular, deontological).
narrative_ontology:cs_reference_frame('211d1573-bbd6-4b55-9da2-cd0b0b4ca399', covenantal_obligation_framework).
narrative_ontology:cs_drift_state('211d1573-bbd6-4b55-9da2-cd0b0b4ca399', contemporary_secular_political_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('211d1573-bbd6-4b55-9da2-cd0b0b4ca399', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(jewish_self_determination__religious_covenant_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, religious_zionist_movement).
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, settlement_enterprise).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, secular_zionist_negotiators).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, palestinian_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and advances the religious covenant claim as the foundational basis for Jewish sovereignty and territorial settlement. Operates through educational, legal, and political institutions to institutionalize the religious reading as the legitimate frame for Jewish self-determination. Draws authority from Torah interpretation and rabbinic tradition; sees territorial control as a religious obligation, not a matter of pragmatic politics or compromise.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, religious_zionist_movement, agenda_setter,
    organized, civilizational, identity_locked, global).

% Operates under the religious covenant framing to justify and expand residential settlement in contested territories. Receives institutional support, legal recognition, and resource allocation justified by the religious claim. The enterprise's continuation depends on the religious reading remaining institutionally dominant; territorial expansion is framed as fulfilling divine obligation rather than political appropriation.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, settlement_enterprise, beneficiary,
    organized, generational, identity_locked, regional).

% Operate within a negotiation framework that assumes territorial compromise, land swaps, and two-state or confederal solutions are politically viable. The religious covenant reading constrains their negotiating space by reframing territorial claims as non-negotiable religious obligations; they bear the cost of reduced diplomatic flexibility and heightened international friction, but cannot overtly repudiate the religious frame without fracturing the coalition.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, secular_zionist_negotiators, payer,
    institutional, biographical, constrained, national).

% Face a negotiating counterparty whose territorial claims rest on a religious-obligation frame that treats secular compromise as theological betrayal. The religious reading forecloses the negotiating logic under which mutual land concessions or shared sovereignty would be intelligible to one side; Palestinian negotiators bear the structural cost of asymmetric frameworks—territorial claims presented as secular political interests meet claims framed as divine commandment.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, palestinian_claimants, payer,
    moderate, generational, trapped, regional).

% Argue for Jewish self-determination grounded in universal principles of nationalism and minority rights rather than particularist religious claims. Are systematically marginalized in institutional settings where the religious covenant reading has become hegemonic among certain political factions; their framework (nationality as sufficient justification) is treated as inadequate or theologically compromised by the religious reading's proponents.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, liberal_nationalist_advocates, excluded,
    institutional, biographical, constrained, national).

% Argue that Jewish collective life is viable and preferable outside territorial sovereignty, secured through diaspora pluralism and minority rights. The religious covenant reading treats their position as theologically incoherent (rejecting divine command) and politically dangerous (abandoning Jewish security); they are structurally excluded from authority over interpretations of Jewish obligation and historical claim.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, diasporist_critics, excluded,
    moderate, generational, mobile, global).

% Operates under assumptions of territorial law, secular political negotiation, and universal human rights. Observes a constraint (the religious covenant reading) that operates orthogonally to its own epistemic foundations—a participant framing territorial claims as religious obligations makes the observer's diplomatic and legal tools structurally mismatched to the actual motivation structure. The observer cannot resolve the constraint because the constraint operates in a different authority register.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, international_secular_framework, observer,
    institutional, generational, analytical, universal).

% The institutional and textual apparatus (Torah, rabbinic tradition, contemporary religious authority) that grounds and legitimates the covenant claim. Not an actor collecting benefits, but a non-agent entity that structures the field of legitimate claims and interpretations for the stakeholders who do act.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, religious_authority_structure, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(jewish_self_determination__religious_covenant_reading, religious_authority_structure).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__religious_covenant_reading, religious_zionist_movement).
narrative_ontology:fixing_cost_class(jewish_self_determination__religious_covenant_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for Jewish identity and collective continuity centered on territorial connection and religious obligation. Coordinates Jewish institutional life around the interpretation that sovereignty in a specific land is constitutive of Jewish flourishing and religious duty, rather than contingent political achievement.
% TRANSFER_FUNCTION: Transfers legitimacy from secular political negotiation frameworks to religious-obligation frameworks. Moves territorial claims from the register of pragmatic negotiation (where compromise is intelligible) to the register of divine command (where compromise is theological betrayal). Transfers diaspora Jewish political energy toward territorial sovereignty support and settlement enterprise.
% ABSENT_VOICES: Diasporist voices arguing for Jewish security through pluralism outside territorial sovereignty; liberal nationalist voices arguing the nation-principle alone suffices; Palestinian voices arguing for reciprocal territorial claims under the same religious-obligation frame (e.g., Islamic covenant claims); secular Palestinian negotiators proposing frameworks in which both parties' territorial interests can be politically negotiated rather than theologically absolutized.
% DISAPPEARANCE_RATIONALE: If the religious covenant reading lost institutional dominance, the terrain of Jewish self-determination discourse would reorganize: territorial claims would shift to secular nationalist, indigenous-return, or liberal-rights frames; settlement policy would require secular political justification rather than religious obligation; negotiations with Palestinian counterparts would become linguistically and conceptually possible within a shared secular-political register. The disappearance of THIS reading would not eliminate Jewish nationalism or territorial claims, but would remove the frame that treats territorial sovereignty as a non-negotiable religious obligation.
% FOUNDING_PROBLEM: Ensuring Jewish collective survival and cultural-religious continuity in the diaspora, and providing a framework for understanding Jewish relationship to the land of Israel after two millennia of displacement.
% FOUNDING_PROBLEM_CORROBORATION: Religious Zionist authorities attest the founding problem persists—antisemitism and diaspora vulnerability require territorial sovereignty as a religious imperative. Secular Jewish historians and diasporist critics attest the founding problem has been substantially addressed through alternative means (minority rights, institutional integration, cultural pluralism) and that the religious covenant reading represents a particular ideological choice, not a necessary response. Palestinian scholars and international observers attest the founding problem exists but the religious covenant reading compounds rather than resolves it by rendering negotiation structurally impossible.
narrative_ontology:disappearance_verdict(jewish_self_determination__religious_covenant_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__religious_covenant_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__religious_covenant_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_self_determination__religious_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__religious_covenant_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__religious_covenant_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__religious_covenant_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, ExtMetricName, E),
    domain_priors:suppression_score(jewish_self_determination__religious_covenant_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(jewish_self_determination__religious_covenant_reading),
    narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(jewish_self_determination__religious_covenant_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.82 at endpoint, rising from 0.68): The religious covenant reading transfers territorial claims from the negotiable register (pragmatic politics) to the non-negotiable register (divine command). Within the religious framework alone, extractiveness would be near-zero—the claim would be perceived as natural law, not extraction. But in the contested, multi-framework landscape, the constraint extracts value by rendering secular compromise frameworks incoherent to one negotiating party. The measurement series tracks institutionalization: the reading gains extractive force as it becomes more established in institutional settings, constraining negotiators who operate in secular registers. The asymptotic approach after t=35 reflects stabilization of institutional dominance; further extraction comes through enforcement rather than frame-expansion. SUPPRESSION (0.71 at endpoint, rising from 0.55): The constraint requires active suppression of alternative readings—diasporist, liberal-nationalist, and Palestinian-reciprocal readings must be marginalized in institutional and public discourse for the religious covenant reading to maintain dominance. The rise in suppression_requirement tracks institutional hardening and coercive maintenance. THEATER_RATIO (0.28 at endpoint): The reading performs both real function (coordinates Jewish religious identity and institutional life around territorial claim) and theatrical function (uses religious language to legitimize political-territorial choices). The moderate theater ratio reflects that the theological claim is genuinely held, but also deployed strategically to foreclose negotiation. ACCESSIBILITY_COLLAPSE: High across levels (0.65–0.72 at t0, rising to 0.74–0.80 at t50) because once the religious covenant framing is adopted, alternatives collapse—a religious Jew committed to the Torah's authority cannot intelligibly adopt a secular pragmatic framework that treats the covenant as negotiable. RESISTANCE: Substantial and declining (0.71 at t0, declining to 0.45 at t50 at individual level), reflecting fatigue and marginalization of alternative voices as the reading becomes institutionally dominant.
 *
 * PERSPECTIVAL GAP:
 *   The religious Zionist movement's perceived type: MOUNTAIN (divine command as immutable). Secular negotiators' perceived type: TANGLED ROPE (coordination of Jewish institutional life entangled with extraction of negotiating flexibility). Palestinian claimants' perceived type: SNARE (religious framing as cover for territorial appropriation; coerced by foreclosed alternatives). Liberal nationalists' perceived type: ROPE (genuine coordination of Jewish self-determination achievable without religious obligation; religious covenant reading as unnecessary overlay). Diasporist critics' perceived type: SNARE (religious Zionism as entrapment, tying Jewish fate to militarized sovereignty). The constraint as CLAIMED (MOUNTAIN) and as OPERATIONALIZED (TANGLED ROPE) diverge deliberately: within the religious-authority register alone, it functions as immutable law; in the contested, multi-framework landscape where multiple authority registers meet, it functions as extraction. The measurement series (rising extractiveness, rising suppression, moderate theater) tracks OPERATIONALIZATION—how the constraint behaves when it encounters secular negotiators, Palestinian claimants, and international frameworks that do not recognize the religious authority register as decisive.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious_zionist_movement: d ≈ 0.15 (full beneficiary). Controls the frame, distributes legitimacy to allied actors, collects institutional authority. High power (organized), identity-locked exit (cannot abandon covenant interpretation without abandoning Jewish religious identity as self-understood). Settlement_enterprise: d ≈ 0.20 (beneficiary, slightly higher than movement because contracted to territorial expansion). Receives resource allocation and legal recognition justified by the religious reading. Identity-locked exit (abandoning the reading means abandoning the enterprise's legitimizing narrative). Secular_zionist_negotiators: d ≈ 0.55 (symmetric, leaning toward target). Retain institutional power but constrained negotiating space; bear reputational cost internationally and intra-communal cost from religious movement if they negotiate territory as tradeable. Exit is constrained—cannot overtly repudiate religious coalition without fracturing their political base. Palestinian_claimants: d ≈ 0.88 (full target). Structurally trapped: the religious covenant reading forecloses the negotiating logic under which their territorial claims would be intelligible to the counterparty. No exit options except military resistance or acceptance. Liberal_nationalist_advocates: d ≈ 0.60 (target, moderate). Marginalized from institutional authority; their framework is treated as inadequate. Organized power but constrained by dominance of religious reading in certain institutional settings. Diasporist_critics: d ≈ 0.70 (target, higher). Excluded from authority over interpretation of Jewish obligation; treated as theologically incoherent. Mobile exit (can operate in diaspora intellectual circles) but excluded from domestic institutional settings. International_secular_framework: d ≈ 0.50 (analytical, symmetric). Observes the constraint but its tools do not match the authority registers in which the constraint operates. Can apply pressure but cannot internally resolve the constraint without one party shifting authority registers.
 *
 * MANDATROPHY ANALYSIS:
 *   FOUNDING PROBLEM: Ensuring Jewish collective survival and religious continuity post-diaspora, and providing interpretive framework for relationship to the land after two millennia of displacement. FOUNDING_PROBLEM_STATUS: Contested. Religious Zionist authorities attest it remains live—diaspora vulnerability and antisemitism persist; territorial sovereignty is the continuing imperative. Secular historians and diasporist critics attest the problem has been substantially addressed through minority-rights institutions, cultural integration, and institutional pluralism in the diaspora. Palestinian scholars attest the founding problem exists (Jewish security concerns are real) but the religious covenant reading compounds rather than resolves it by rendering mutual territorial negotiation impossible. MANDATROPHY SIGNAL (disappearance_verdict x founding_problem_status): WORLD_REARRANGES + CONTESTED = the constraint's founding problem is disputed; if the religious covenant reading lost institutional dominance, territorial discourse would reorganize into secular-nationalist, indigenous-return, or liberal-rights frames. The constraint is not yet fully mandatroph (the founding problem has not been conclusively superseded), but shows MANDATROPHY PRESSURE: the religious reading is increasingly detached from pragmatic security functions (territorial control is no longer the only viable path to Jewish security; alternative institutional arrangements exist). The theater_ratio (0.28) remains moderate rather than high because the theological claim retains genuine hold over its constituency; if theater_ratio were rising toward 0.6+, that would signal advanced mandatrophy (the reading maintained purely theatrically, divorced from real function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    religious_authority_recognition,
    'Is the divine covenant claim a natural fact about Jewish religious obligation (a mountain), or a particular institutional interpretation adopted and maintained by certain Jewish movements (a snare/tangled_rope)?',
    'Comparison across Jewish communities and historical periods: Does all Jewish religious tradition affirm that territorial sovereignty is obligatory? Or only certain modern movements? Historical analysis of how the covenant-obligation connection was interpreted in diaspora periods.',
    'If the interpretation is universal and unchanging, the reading functions as mountain within the religious register. If it is particular and historically contingent, the reading is better understood as an institutional choice that benefits settlement enterprise and religious Zionist movement—a tangled_rope riding religious authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_authority_recognition, empirical, 'Whether the divine covenant claim is obligatory or interpretively particular.').

omega_variable(
    authority_register_foreclosure,
    'Does the religious covenant reading logically foreclose secular-pragmatic territorial negotiation, or do they coexist as parallel frameworks?',
    'Interview and ethnographic study: Can individuals simultaneously hold that (1) the land is divinely covenanted to the Jewish people AND (2) pragmatic territorial compromise is negotiable? Or does commitment to (1) functionally disable assent to (2)?',
    'If the frameworks genuinely coexist (agents hold both), the reading coexists_with secular frameworks. If commitment to religious reading functionally disables secular negotiation, the reading forecloses it—the foreclosure becomes the mechanism of extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authority_register_foreclosure, empirical, 'Whether religious and secular frameworks can simultaneously structure the same agent''s territorial position.').

omega_variable(
    suppression_internalization_ambiguity,
    'Is the suppression of diasporist, liberal-nationalist, and Palestinian readings structural (institutional marginalization, access barriers) or internalized (the readings feel theologically incoherent from within the religious framework)?',
    'Post-exit trajectory: If religious Zionists exit the religious framework (secularize, migrate, change communities), does suppression of alternative readings persist? If it persists, suppression is partly internalized; if it dissolves, suppression is primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests—the suppression travels with individuals who leave the institutional setting. If structural, the suppression is reversible through institutional reform without requiring individual identity change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_ambiguity, empirical, 'Structural vs. internalized suppression mechanism.').

omega_variable(
    referent_ambiguity,
    'What is the ε referent for this reading? Is it (A) the standing arrangement under contest from the reading''s own lights (the covenant claim as the reading sees it—ε near 0.0, divine obligation), or (B) the standing arrangement from the perspective of parties NOT sharing the religious authority register (the covenant claim as a constraint imposed on secular negotiators—ε high)?',
    'Clarify whether the engine should compute χ from the reading''s endorsed authority register (where the religious covenant is natural law, ε ≈ 0.0) or from the multi-framework landscape (where the reading''s dominance constrains secular actors, ε ≈ 0.82). OQ-26 declares ε is reading-indexed over a fixed referent; the ambiguity is which seat''s reading frames the referent.',
    'If the reading''s own authority register is the standpoint: ε ≈ 0.0 (natural law), classification trends toward mountain, suppression appears low, per-seat divergence is minimized. If the contested multi-framework landscape is the standpoint: ε ≈ 0.82 (imposed constraint), classification trends toward tangled_rope/snare, suppression appears high, per-seat divergence is maximized. The authored ε (0.82) assumes the multi-framework standpoint; resolving the ambiguity determines whether this choice is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(referent_ambiguity, conceptual, 'Frame-indexing of ε: religious authority register vs. contested multi-framework landscape.').

omega_variable(
    settler_colonial_reading_foreclosure,
    'Does the religious covenant reading logically foreclose the settler-colonial reading, or do they remain live alternative interpretations of the same territorial history?',
    'Logical analysis: Can the same acts (displacement, settlement, institutional marginalization of Palestinians) be simultaneously described as ''fulfilling divine covenant'' and ''implementing settler-colonial dispossession''? If yes, coexistence. If no, foreclosure.',
    'If foreclosure: the religious reading and settler-colonial reading cannot coexist in a single framework; one must be rejected for the other to hold. If coexistence: both remain live interpretations; observers can recognize both simultaneously (different constituencies adopt different readings; no logical constraint prevents both being true descriptions of the same events).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settler_colonial_reading_foreclosure, conceptual, 'Logical relationship between religious covenant and settler-colonial interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__religious_covenant_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_self_determination__religious_covenant_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(jewi_tr_t7, jewish_self_determination__religious_covenant_reading, theater_ratio, 7, 0.2).
narrative_ontology:measurement(jewi_tr_t14, jewish_self_determination__religious_covenant_reading, theater_ratio, 14, 0.23).
narrative_ontology:measurement(jewi_tr_t21, jewish_self_determination__religious_covenant_reading, theater_ratio, 21, 0.25).
narrative_ontology:measurement(jewi_tr_t28, jewish_self_determination__religious_covenant_reading, theater_ratio, 28, 0.27).
narrative_ontology:measurement(jewi_tr_t35, jewish_self_determination__religious_covenant_reading, theater_ratio, 35, 0.28).
narrative_ontology:measurement(jewi_tr_t42, jewish_self_determination__religious_covenant_reading, theater_ratio, 42, 0.28).
narrative_ontology:measurement(jewi_tr_t50, jewish_self_determination__religious_covenant_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_self_determination__religious_covenant_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(jewi_be_t7, jewish_self_determination__religious_covenant_reading, base_extractiveness, 7, 0.72).
narrative_ontology:measurement(jewi_be_t14, jewish_self_determination__religious_covenant_reading, base_extractiveness, 14, 0.76).
narrative_ontology:measurement(jewi_be_t21, jewish_self_determination__religious_covenant_reading, base_extractiveness, 21, 0.79).
narrative_ontology:measurement(jewi_be_t28, jewish_self_determination__religious_covenant_reading, base_extractiveness, 28, 0.81).
narrative_ontology:measurement(jewi_be_t35, jewish_self_determination__religious_covenant_reading, base_extractiveness, 35, 0.82).
narrative_ontology:measurement(jewi_be_t42, jewish_self_determination__religious_covenant_reading, base_extractiveness, 42, 0.82).
narrative_ontology:measurement(jewi_be_t50, jewish_self_determination__religious_covenant_reading, base_extractiveness, 50, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_self_determination__religious_covenant_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(jewi_su_t7, jewish_self_determination__religious_covenant_reading, suppression_requirement, 7, 0.61).
narrative_ontology:measurement(jewi_su_t14, jewish_self_determination__religious_covenant_reading, suppression_requirement, 14, 0.65).
narrative_ontology:measurement(jewi_su_t21, jewish_self_determination__religious_covenant_reading, suppression_requirement, 21, 0.68).
narrative_ontology:measurement(jewi_su_t28, jewish_self_determination__religious_covenant_reading, suppression_requirement, 28, 0.7).
narrative_ontology:measurement(jewi_su_t35, jewish_self_determination__religious_covenant_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement(jewi_su_t42, jewish_self_determination__religious_covenant_reading, suppression_requirement, 42, 0.71).
narrative_ontology:measurement(jewi_su_t50, jewish_self_determination__religious_covenant_reading, suppression_requirement, 50, 0.71).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=50
narrative_ontology:measurement(jewi_grid_01, jewish_self_determination__religious_covenant_reading, accessibility_collapse(class), 0, 0.71).
narrative_ontology:measurement(jewi_grid_02, jewish_self_determination__religious_covenant_reading, accessibility_collapse(class), 50, 0.79).
narrative_ontology:measurement(jewi_grid_03, jewish_self_determination__religious_covenant_reading, accessibility_collapse(individual), 0, 0.65).
narrative_ontology:measurement(jewi_grid_04, jewish_self_determination__religious_covenant_reading, accessibility_collapse(individual), 50, 0.74).
narrative_ontology:measurement(jewi_grid_05, jewish_self_determination__religious_covenant_reading, accessibility_collapse(organizational), 0, 0.68).
narrative_ontology:measurement(jewi_grid_06, jewish_self_determination__religious_covenant_reading, accessibility_collapse(organizational), 50, 0.76).
narrative_ontology:measurement(jewi_grid_07, jewish_self_determination__religious_covenant_reading, accessibility_collapse(structural), 0, 0.72).
narrative_ontology:measurement(jewi_grid_08, jewish_self_determination__religious_covenant_reading, accessibility_collapse(structural), 50, 0.8).
narrative_ontology:measurement(jewi_grid_09, jewish_self_determination__religious_covenant_reading, resistance(class), 0, 0.58).
narrative_ontology:measurement(jewi_grid_10, jewish_self_determination__religious_covenant_reading, resistance(class), 50, 0.53).
narrative_ontology:measurement(jewi_grid_11, jewish_self_determination__religious_covenant_reading, resistance(individual), 0, 0.52).
narrative_ontology:measurement(jewi_grid_12, jewish_self_determination__religious_covenant_reading, resistance(individual), 50, 0.45).
narrative_ontology:measurement(jewi_grid_13, jewish_self_determination__religious_covenant_reading, resistance(organizational), 0, 0.65).
narrative_ontology:measurement(jewi_grid_14, jewish_self_determination__religious_covenant_reading, resistance(organizational), 50, 0.61).
narrative_ontology:measurement(jewi_grid_15, jewish_self_determination__religious_covenant_reading, resistance(structural), 0, 0.71).
narrative_ontology:measurement(jewi_grid_16, jewish_self_determination__religious_covenant_reading, resistance(structural), 50, 0.68).
narrative_ontology:measurement(jewi_grid_17, jewish_self_determination__religious_covenant_reading, stakes_inflation(class), 0, 0.55).
narrative_ontology:measurement(jewi_grid_18, jewish_self_determination__religious_covenant_reading, stakes_inflation(class), 50, 0.7).
narrative_ontology:measurement(jewi_grid_19, jewish_self_determination__religious_covenant_reading, stakes_inflation(individual), 0, 0.48).
narrative_ontology:measurement(jewi_grid_20, jewish_self_determination__religious_covenant_reading, stakes_inflation(individual), 50, 0.62).
narrative_ontology:measurement(jewi_grid_21, jewish_self_determination__religious_covenant_reading, stakes_inflation(organizational), 0, 0.62).
narrative_ontology:measurement(jewi_grid_22, jewish_self_determination__religious_covenant_reading, stakes_inflation(organizational), 50, 0.77).
narrative_ontology:measurement(jewi_grid_23, jewish_self_determination__religious_covenant_reading, stakes_inflation(structural), 0, 0.58).
narrative_ontology:measurement(jewi_grid_24, jewish_self_determination__religious_covenant_reading, stakes_inflation(structural), 50, 0.73).
narrative_ontology:measurement(jewi_grid_25, jewish_self_determination__religious_covenant_reading, suppression(class), 0, 0.48).
narrative_ontology:measurement(jewi_grid_26, jewish_self_determination__religious_covenant_reading, suppression(class), 50, 0.64).
narrative_ontology:measurement(jewi_grid_27, jewish_self_determination__religious_covenant_reading, suppression(individual), 0, 0.42).
narrative_ontology:measurement(jewi_grid_28, jewish_self_determination__religious_covenant_reading, suppression(individual), 50, 0.58).
narrative_ontology:measurement(jewi_grid_29, jewish_self_determination__religious_covenant_reading, suppression(organizational), 0, 0.58).
narrative_ontology:measurement(jewi_grid_30, jewish_self_determination__religious_covenant_reading, suppression(organizational), 50, 0.73).
narrative_ontology:measurement(jewi_grid_31, jewish_self_determination__religious_covenant_reading, suppression(structural), 0, 0.52).
narrative_ontology:measurement(jewi_grid_32, jewish_self_determination__religious_covenant_reading, suppression(structural), 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__diasporist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'jewish_self_determination'. All five readings (religious_covenant, liberal_nationalist, indigenous_return, settler_colonial, diasporist) share the same referent (Jewish collective relationship to territorial sovereignty) but diverge on authority register, grounding, and structural consequences. The religious_covenant_reading instantiates a commitment-system constraint grounded in Torah and rabbinic tradition; it forecloses the liberal_nationalist and diasporist readings (which ground legitimacy in secular principles orthogonal to religious obligation) and coexists with the settler_colonial and indigenous_return readings (which describe the same territorial events under different interpretive frames). Each reading is authored as a separate constraint with its own ε, beneficiary/victim structure, and per-seat classifications. The network edges enable contamination analysis: institutional dominance of the religious_covenant reading constrains the negotiating space for secular_zionist_negotiators and Palestinian claimants, which cascades through institutional and diplomatic channels to affect the viability of liberal_nationalist and diasporist alternatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_self_determination__religious_covenant_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
