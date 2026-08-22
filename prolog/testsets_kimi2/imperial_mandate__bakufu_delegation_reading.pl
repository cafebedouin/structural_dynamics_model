% ============================================================================
% CONSTRAINT STORY: imperial_mandate__bakufu_delegation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imperial_mandate__bakufu_delegation_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: imperial_mandate__bakufu_delegation_reading
 *   human_readable: Bakufu Delegation Reading of the Imperial Mandate
 *   domain: political/philosophical/historical
 *
 * SUMMARY:
 *   This constraint models the bakufu delegation reading of the imperial
 *   mandate kernel: the political-theological arrangement in which the
 *   Japanese emperor retains divine legitimacy and ritual sovereignty while
 *   actual administrative, military, and fiscal authority is delegated to the
 *   shogun and the samurai class. The bifurcation allows warrior governments
 *   to rule without claiming the throne, while the imperial house is
 *   sustained as a symbolic font of legitimacy whose direct political
 *   involvement is actively suppressed. The constraint persisted across
 *   multiple regime changes (Kamakura, Muromachi, Tokugawa) and only
 *   collapsed when the Meiji Restoration repudiated the delegation model in
 *   favor of direct imperial rule. This story instantiates ONE reading of a
 *   contested kernel; the sibling loyalist_restoration_reading holds that
 *   legitimacy is inseparable from active imperial governance.
 *
 * KEY AGENTS:
 *   - imperial_house: Primary target (moderate/identity_locked) â divine legitimacy is harvested while political agency is suppressed
 *   - bakufu_administration: Primary agenda-setter and capturer (institutional/arbitrage) â exercises authority and enforces the bifurcation
 *   - samurai_class: Primary beneficiary (organized/constrained) â constituted as legitimate governing stratum through delegated mandate
 *   - agrarian_commoners: Secondary target (powerless/trapped) â subject to warrior governance without imperial recourse
 *   - loyalist_scholars: Excluded voice (moderate/constrained) â advocate direct imperial rule, structurally marginalized
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_mandate__bakufu_delegation_reading, 0.72).
domain_priors:suppression_score(imperial_mandate__bakufu_delegation_reading, 0.72).
domain_priors:theater_ratio(imperial_mandate__bakufu_delegation_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__bakufu_delegation_reading, tangled_rope).
narrative_ontology:human_readable(imperial_mandate__bakufu_delegation_reading, "Bakufu Delegation Reading of the Imperial Mandate").
narrative_ontology:topic_domain(imperial_mandate__bakufu_delegation_reading, "political/philosophical/historical").

domain_priors:requires_active_enforcement(imperial_mandate__bakufu_delegation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__bakufu_delegation_reading, '570ce3bf-857f-475e-abab-ec2b1a19f89c').
narrative_ontology:cs_kernel_codification('570ce3bf-857f-475e-abab-ec2b1a19f89c', implicit).
narrative_ontology:cs_authority_grounding('570ce3bf-857f-475e-abab-ec2b1a19f89c', extraction).
narrative_ontology:cs_interpretation_layer_present('570ce3bf-857f-475e-abab-ec2b1a19f89c').
narrative_ontology:cs_reading_relation('570ce3bf-857f-475e-abab-ec2b1a19f89c', imperial_mandate__loyalist_restoration_reading, forecloses).
narrative_ontology:cs_axiom('570ce3bf-857f-475e-abab-ec2b1a19f89c', foundational, divine_mandate_permits_institutional_delegation).
narrative_ontology:cs_axiom_status(divine_mandate_permits_institutional_delegation, holdable).
narrative_ontology:cs_axiom_grounding('570ce3bf-857f-475e-abab-ec2b1a19f89c', divine_mandate_permits_institutional_delegation, theological).
narrative_ontology:cs_axiom('570ce3bf-857f-475e-abab-ec2b1a19f89c', foundational, samurai_governance_legitimate_through_ritual_sovereignty).
narrative_ontology:cs_axiom_status(samurai_governance_legitimate_through_ritual_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('570ce3bf-857f-475e-abab-ec2b1a19f89c', samurai_governance_legitimate_through_ritual_sovereignty, conventional).
narrative_ontology:cs_reference_frame('570ce3bf-857f-475e-abab-ec2b1a19f89c', ritual_political_bifurcation).
narrative_ontology:cs_drift_state('570ce3bf-857f-475e-abab-ec2b1a19f89c', imperial_restoration_crisis, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('570ce3bf-857f-475e-abab-ec2b1a19f89c', '').
narrative_ontology:cs_kernel_id(imperial_mandate__bakufu_delegation_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, bakufu_administration).
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, samurai_class).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, imperial_house).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, agrarian_commoners).
narrative_ontology:constraint_vindicates(imperial_mandate__bakufu_delegation_reading, bifurcated_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(imperial_mandate__bakufu_delegation_reading, samurai_governance_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains divine status and ritual sovereignty but is systematically excluded from political decision-making. Political agency is suppressed; the household's identity is fused with the imperial institution, making exit impossible without dissolving the lineage's cosmic role. Receives material support in exchange for legitimating delegations of authority.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, imperial_house, payer,
    moderate, civilizational, identity_locked, national).

% Exercises monopoly on military and administrative governance. Enforces the separation between ritual legitimacy and political authority. Maintains the emperor as a symbolic font of legitimacy while capturing all substantive governing power, taxation, and military command. Actively suppresses direct imperial rule and loyalist movements.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, bakufu_administration, agenda_setter,
    institutional, generational, arbitrage, national).

% Constituted as the legitimate governing stratum by virtue of delegated imperial mandate. Receives stipends, status privileges, and monopoly on arms and office-holding. Their social identity and economic position depend on the bifurcation persisting across regime changes.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, samurai_class, beneficiary,
    organized, biographical, constrained, national).

% Subject to land taxation, corvÃ©e labor, and warrior-class justice without recourse to a higher imperial authority. The bifurcation removes the theoretical appeal to the emperor as direct sovereign, leaving them with only local samurai administrators.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, agrarian_commoners, payer,
    powerless, immediate, trapped, local).

% Advocate for unmediated imperial rule based on classical political and religious texts. Are politically marginalized, sometimes persecuted, and denied institutional voice because their position threatens the delegation structure and the warrior monopoly on governance.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, loyalist_scholars, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imperial_mandate__bakufu_delegation_reading, bakufu_administration).
narrative_ontology:fixing_cost_class(imperial_mandate__bakufu_delegation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the legitimacy crisis of military rule in a polity that predicates sovereignty on an unbroken divine imperial lineage by separating ritual legitimacy from administrative authority, allowing warrior governments to govern without abolishing the imperial institution or triggering endemic civil war over the throne.
% TRANSFER_FUNCTION: Transfers substantive political authority, military command, taxation, and judicial power from the imperial house to the bakufu and samurai class; transfers legitimacy upward from the emperor to the shogunate through ritual investiture and symbolic endorsement.
% ABSENT_VOICES: Loyalist court factions and classical scholars who hold that sovereignty is indivisible and that the emperor must exercise direct rule; they are excluded from bakufu councils and often suppressed because their position threatens the delegation structure.
% DISAPPEARANCE_RATIONALE: If the bifurcation vanished and the imperial house reclaimed direct governance, the warrior administrative apparatus would lose its constitutional foundation; samurai status privileges would require entirely new legitimation; the polity would face either a return to direct imperial rule or a vacuum of authority followed by civil war.
% FOUNDING_PROBLEM: How to legitimate the actual governance of military elites in a political culture that predicates sovereignty on a divine imperial lineage descended from celestial origins, avoiding both endless civil war over the throne and the abolition of the imperial institution.
% FOUNDING_PROBLEM_CORROBORATION: Modern historians and political theorists outside the beneficiary classes attest that the original legitimacy crisis was substantially resolved by the bifurcation but that the arrangement persisted as a rent-preserving shell long after the founding coordination need had been superseded by social change. No impartial corroboration from within the Tokugawa beneficiary apparatus exists; the bakufu's own historians asserted the problem remained live to justify continued warrior dominance.
narrative_ontology:disappearance_verdict(imperial_mandate__bakufu_delegation_reading, world_rearranges).
narrative_ontology:founding_problem_status(imperial_mandate__bakufu_delegation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__bakufu_delegation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imperial_mandate__bakufu_delegation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imperial_mandate__bakufu_delegation_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imperial_mandate__bakufu_delegation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imperial_mandate__bakufu_delegation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imperial_mandate__bakufu_delegation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the arrangement systematically transfers political authority, military command, and surplus extraction from the ritual center to the warrior administration. Suppression (0.72) is comparably high because the constraint depends on actively excluding the imperial house from governance and marginalizing loyalist scholars who would restore direct rule. Theater_ratio (0.55) reflects the increasing ritualization of imperial functions and the performative maintenance of a sovereignty bifurcation that had become more symbolic than substantive by the late Tokugawa period. Accessibility_collapse (0.82) captures how alternatives to bifurcated sovereignty became nearly unthinkable within the political imagination for centuries. Resistance (0.35) is moderate because loyalist movements were historically sporadic and successfully suppressed until the terminal crisis of the bakufu system.
 *
 * PERSPECTIVAL GAP:
 *   The imperial house experiences this constraint as a suppression of its governing function and a reduction to ritual performance; its identity-locked position (there is no exit from being the imperial line) amplifies effective extraction. The bakufu administration experiences the same structure as a necessary coordination mechanism that prevents civil war over the throne while capturing administrative authority. The samurai class experiences it as a status entitlement grounded in delegated legitimacy. The engine will compute divergent per-seat types: the imperial house and commoners as high-target seats, the bakufu as near-beneficiary, and the samurai class as moderate-beneficiary.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the bakufu_administration and samurai_class: they collect political authority, economic rents, and status privileges through the delegation structure. Victims are the imperial_house and agrarian_commoners: the former loses political agency to the delegation mechanism, the latter loses the theoretical recourse to imperial justice and bears the extractive burden of samurai governance. The imperial house's identity_locked exit and the commoners' trapped status push their directionality toward the full-target end, while the bakufu's arbitrage-grade exit and institutional power push it toward the beneficiary end. No directionality overrides are needed because the structural derivation captures the relationships accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The R5 genealogy interview prevents mislabeling this constraint as a pure snare or a pure rope. The founding problem â how to legitimate military rule in a culture centered on divine imperial lineage â was a real coordination problem. The bifurcation provided a genuine solution that prevented succession wars and maintained institutional continuity. However, the founding_problem_status is 'dead' because by the late Tokugawa period the arrangement had become primarily extractive: the bakufu maintained the ritual shell to preserve warrior privilege long after the original coordination need had mutated. The mismatch between dead founding_problem and world_rearranges disappearance_verdict flags the constraint as a captured/zombie structure, consistent with its tangled_rope classification. Without the R5 interview, the constraint might be misread as either pure coordination (ignoring the suppression of the imperial house) or pure extraction (ignoring the genuine early legitimacy function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer,
    'Does the bakufu delegation reading capture a pre-existing theological structure of the imperial mandate, or is it an ex post legitimization of military usurpation?',
    'Historical-textual analysis of pre-bakufu imperial political theory versus bakufu-era ideological production; comparison with other East Asian mandate-of-heaven traditions that lack delegation doctrines.',
    'If the delegation reading is a post-hoc construction, the constraint''s coordination story functions as cover for extraction and classification shifts toward snare; if it reflects genuine theological development, the coordination component is structurally grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer, conceptual, 'Whether the delegation reading is discovered or invented').

omega_variable(
    divine_mandate_naturality,
    'Is the imperial divine mandate a pre-political theological constant or a politically constructed narrative whose natural-law appearance serves the bifurcation?',
    'Comparative analysis of imperial ideology across the Nara, Heian, and Kamakura periods to determine whether divine mandate theology predates warrior rule or was retrofitted to it.',
    'If purely constructed, the constraint loses its mountain-flavored immunity and reads as a more extractive tangled rope or snare; if genuinely pre-political, the bifurcation has a stronger coordination claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_mandate_naturality, conceptual, 'Natural-law versus constructed status of the divine mandate').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the imperial house''s political passivity produced by structural suppression (bakufu military dominance and court poverty) or by internalized identity fusion (the emperor as ritual-being-unto-itself)?',
    'Post-Meiji trajectory analysis: did imperial political ambition immediately revive once structural suppression was removed, or did the household require decades to reconceptualize itself as a governing actor?',
    'If internalized, effective suppression exceeds the structural measure because the target carried the constraint after exit; this raises the computed extraction for the imperial house seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of imperial political agency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__bakufu_delegation_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impe_tr_t0, imperial_mandate__bakufu_delegation_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(impe_tr_t100, imperial_mandate__bakufu_delegation_reading, theater_ratio, 100, 0.35).
narrative_ontology:measurement(impe_tr_t200, imperial_mandate__bakufu_delegation_reading, theater_ratio, 200, 0.4).
narrative_ontology:measurement(impe_tr_t300, imperial_mandate__bakufu_delegation_reading, theater_ratio, 300, 0.45).
narrative_ontology:measurement(impe_tr_t400, imperial_mandate__bakufu_delegation_reading, theater_ratio, 400, 0.5).
narrative_ontology:measurement(impe_tr_t500, imperial_mandate__bakufu_delegation_reading, theater_ratio, 500, 0.55).

% Extraction over time
narrative_ontology:measurement(impe_be_t0, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(impe_be_t100, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 100, 0.55).
narrative_ontology:measurement(impe_be_t200, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 200, 0.6).
narrative_ontology:measurement(impe_be_t300, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 300, 0.65).
narrative_ontology:measurement(impe_be_t400, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 400, 0.68).
narrative_ontology:measurement(impe_be_t500, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 500, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(impe_su_t0, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(impe_su_t100, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 100, 0.55).
narrative_ontology:measurement(impe_su_t200, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 200, 0.6).
narrative_ontology:measurement(impe_su_t300, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 300, 0.68).
narrative_ontology:measurement(impe_su_t400, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 400, 0.7).
narrative_ontology:measurement(impe_su_t500, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 500, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_mandate__bakufu_delegation_reading, identity_coordination).
narrative_ontology:affects_constraint(imperial_mandate__bakufu_delegation_reading, loyalist_restoration_reading).

% DUAL FORMULATION NOTE:
% The imperial_mandate kernel decomposes into two structurally distinct readings. The bakufu_delegation_reading (this file) models bifurcated sovereignty with delegable legitimacy and asymmetric extraction. The loyalist_restoration_reading models indivisible sovereignty requiring direct imperial governance. Each reading produces a different epsilon, different beneficiary/victim structures, and different classification. They are linked as siblings in a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
