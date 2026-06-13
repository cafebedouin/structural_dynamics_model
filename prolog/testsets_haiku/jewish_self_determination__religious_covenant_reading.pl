% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__religious_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jewish_self_determination__religious_covenant_reading
 *   human_readable: Jewish Self-Determination via Divine Covenant
 *   domain: political_philosophy/nationalism_studies/religious_authority
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested
 *   jewish_self_determination kernel: the religious_covenant_reading. The
 *   constraint claims that Jewish territorial sovereignty derives from divine
 *   covenant, making the claim to the land a religious obligation independent
 *   of and superior to secular political frameworks. Within this reading's
 *   own theological framework, the constraint operates as a mountain — divine
 *   command is immutable and non-negotiable. However, because the religious
 *   reading itself is contested (sibling readings assert indigenous return,
 *   liberal nationalism, diasporism, or settler colonialism), the effective
 *   epsilon of the claim in the broader political discourse is substantially
 *   higher than the theological framework alone suggests. The measured
 *   extractiveness reflects this structural contestation: the covenant
 *   reading forecloses secular compromise space, suppresses alternative
 *   Jewish voices, and benefits identifiable institutional actors (religious
 *   Zionist establishment, settlement movement) while victimizing others
 *   (secular compromise proponents, Palestinians excluded from negotiation).
 *   The constraint exhibits rising extractiveness and suppression over the
 *   interval as the religious obligation narrative has been progressively
 *   weaponized to justify territorial expansion.
 *
 * KEY AGENTS:
 *   - religious_zionist_establishment: Agenda-setter (institutional power, identity-locked) — interprets and enforces the covenant claim; their institutional identity is constituted by this interpretation.
 *   - settlement_movement: Beneficiary (organized power, identity-locked) — expands territorial presence justified by covenant obligation; members' personal identities fused with settlement narrative.
 *   - secular_israeli_state: Institutional beneficiary constrained by entanglement — gains legitimacy from religious claim but cannot exit without losing coalition support.
 *   - diaspora_jewish_communities: Payer (organized power, constrained exit) — bear reputational costs and identity constraints from the covenant reading's claim to represent all Judaism.
 *   - palestinian_territorial_claimants: Excluded (moderate power, trapped) — structurally barred from negotiation because religious obligation forecloses secular compromise frameworks.
 *   - secular_compromise_framework: Victim (moderate power, constrained) — the entire secular negotiation space is disabled by assertion of non-negotiable religious obligation.
 *   - international_legal_authority: Observer (institutional, analytical) — operates in secular frameworks while the constraint escapes its jurisdiction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__religious_covenant_reading, 0.82).
domain_priors:suppression_score(jewish_self_determination__religious_covenant_reading, 0.79).
domain_priors:theater_ratio(jewish_self_determination__religious_covenant_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__religious_covenant_reading, mountain).
narrative_ontology:human_readable(jewish_self_determination__religious_covenant_reading, "Jewish Self-Determination via Divine Covenant").
narrative_ontology:topic_domain(jewish_self_determination__religious_covenant_reading, "political_philosophy/nationalism_studies/religious_authority").

domain_priors:requires_active_enforcement(jewish_self_determination__religious_covenant_reading).
domain_priors:emerges_naturally(jewish_self_determination__religious_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__religious_covenant_reading, '3c3ae45c-9313-49fb-bff9-b7a168584b45').
narrative_ontology:cs_kernel_codification('3c3ae45c-9313-49fb-bff9-b7a168584b45', fixed_text).
narrative_ontology:cs_authority_grounding('3c3ae45c-9313-49fb-bff9-b7a168584b45', lineage).
narrative_ontology:cs_interpretation_layer_present('3c3ae45c-9313-49fb-bff9-b7a168584b45').
narrative_ontology:cs_reading_relation('3c3ae45c-9313-49fb-bff9-b7a168584b45', jewish_self_determination__diasporist_reading, forecloses).
narrative_ontology:cs_reading_relation('3c3ae45c-9313-49fb-bff9-b7a168584b45', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('3c3ae45c-9313-49fb-bff9-b7a168584b45', jewish_self_determination__indigenous_return_reading, influences).
narrative_ontology:cs_reading_relation('3c3ae45c-9313-49fb-bff9-b7a168584b45', jewish_self_determination__settler_colonial_reading, coexists_with).
narrative_ontology:cs_axiom('3c3ae45c-9313-49fb-bff9-b7a168584b45', foundational, covenant_obligation_political_binding).
narrative_ontology:cs_axiom_status(covenant_obligation_political_binding, holdable).
narrative_ontology:cs_axiom_grounding('3c3ae45c-9313-49fb-bff9-b7a168584b45', covenant_obligation_political_binding, theological).
narrative_ontology:cs_axiom('3c3ae45c-9313-49fb-bff9-b7a168584b45', foundational, religious_authority_supremacy_over_secular_framework).
narrative_ontology:cs_axiom_status(religious_authority_supremacy_over_secular_framework, holdable).
narrative_ontology:cs_axiom_grounding('3c3ae45c-9313-49fb-bff9-b7a168584b45', religious_authority_supremacy_over_secular_framework, deontological).
narrative_ontology:cs_reference_frame('3c3ae45c-9313-49fb-bff9-b7a168584b45', covenant_obligation_as_binding_political_command).
narrative_ontology:cs_drift_state('3c3ae45c-9313-49fb-bff9-b7a168584b45', contemporary_territorial_expansion_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3c3ae45c-9313-49fb-bff9-b7a168584b45', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__religious_covenant_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, religious_zionist_movement).
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, settlement_enterprise).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, secular_compromise_framework).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, palestinian_territorial_claims).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__religious_covenant_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(jewish_self_determination__religious_covenant_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__religious_covenant_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__religious_covenant_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

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
 *   The measured extractiveness (0.82 at interval end) reflects not the theological claim itself (which within the religious framework is non-extractive — it is divine command) but the political-discursive extraction: the covenant reading extracts by foreclosing secular negotiation space, by capturing Jewish institutional authority and fusing it with settlement expansion, and by marginalizing alternative Jewish voices. The suppression measure (0.79) captures the active enforcement required to maintain the covenant reading against competing interpretations: secular compromise frameworks must be actively delegitimized, diasporist and liberal-secular Jewish positions must be marginalized, and Palestinian voices must be excluded from the negotiation frame. Theater_ratio (0.28) is moderate-low because the religious authority genuinely believes the theological claim and is not performing; however, the escalating use of the covenant frame to justify political expansion (rather than merely to express religious identity) introduces increasing performative use of the theological claim. The accessibility_collapse measure (0.71) reflects that once the covenant claim is accepted within the religious frame, alternatives collapse — but the religious frame itself is not universally accepted, leaving substantial resistance (0.88). The rising trajectory of extractiveness from 0.45 to 0.82 over the interval mirrors the progressive tightening of the religious reading's political grip: as institutional power accumulates around the covenant claim, the settlement movement expands, and secular alternatives are progressively delegitimized, the constraint's extractive function intensifies.
 *
 * PERSPECTIVAL GAP:
 *   The religious_zionist_establishment and settlement_movement seats compute the constraint as a non-extractive mountain (divine obligation is immutable and legitimate). The secular_israeli_state computes it as tangled_rope (coordination of Jewish identity with territorial claims, but entanglement with religious authority that prevents secular exit). The secular_compromise_framework and palestinian seats compute it as snare (religious authority is cover for territorial extraction; their negotiation space is suppressed). The international_legal_authority computes it as a category error or contested claim outside its epistemic jurisdiction. The engine computes per-seat type from the structural data; this perspectival gap is the divergence to measure.
 *
 * DIRECTIONALITY LOGIC:
 *   religious_zionist_establishment: d near 0.0 (full beneficiary — controls interpretation, sets agenda, identity fused with constraint, zero exit cost within framework). settlement_movement: d near 0.15 (beneficiary but organized, slightly constrained by need to maintain religious narrative coherence). secular_israeli_state: d near 0.45 (symmetric but constrained — benefits from legitimacy, pays by having to maintain entanglement and cannot pursue secular compromise). diaspora_jewish_communities: d near 0.65 (payer — bear reputational cost and identity constraints, slight benefit from state security). secular_compromise_framework proponents: d near 0.88 (target — their entire negotiation space is disabled, identity_locked into a framework that forecloses them). palestinian_territorial_claimants: d near 1.0 (full target — trapped, excluded, territorial claims made non-negotiable by religious obligation). international_legal_authority: d analytical (0.5) — observes the constraint's operation but does not collect or pay.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (Jewish historical vulnerability and dispersion) is contested in status: religious authorities assert it is live (Jewish safety requires territorial sovereignty); secular observers and Palestinian authorities assert it is dead (modern Israel exists as a state; vulnerability is now about regional militarization, not about homelessness). This founding_problem_status=contested paired with disappearance_verdict=contested triggers a mandatrophy flag: the constraint may be a zombie — the organizing problem it was built for is no longer live, but the arrangement persists because institutional actors are invested in it. The rising theater_ratio (0.08 → 0.28) supports this: the theological claim is being increasingly instrumentalized for political expansion, suggesting the religious function has atrophied while performative maintenance escalates. The classification divergence (claimed=mountain, measured=tangled_rope/snare per seat) further supports mandatrophy reading: the constraint claims to be natural law (immutable divine covenant) but operates as enforced extraction (suppression rises from 0.55 to 0.79, extractiveness from 0.45 to 0.82) — a false summit candidate. The false_summit_mountain signature should fire because beneficiaries are declared and measurement profiles show rising extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_framework_contestation,
    'Is the covenant reading a binding religious obligation that must structure political action, or is it one theological interpretation among many, none of which should determine state policy?',
    'Systematic collection of religious authorities'' and theologians'' statements on the bindingness of territorial obligation. Examination of Jewish interpretive traditions across centuries to establish whether covenant-as-political-obligation is majority or minority reading historically.',
    'If it is minority reading, the constraint reclassifies from mountain (natural) to snare (enforced minority claim). If it is genuinely binding within Jewish theology, the constraint remains mountain but the extraction measurement becomes the key diagnostic — if a true mountain, why rising suppression? If suppression rises, either the mountain claim is false or the constraint has been weaponized beyond its theological scope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theological_framework_contestation, empirical, 'Whether the covenant obligation reading is a binding religious claim or a contested interpretation.').

omega_variable(
    framework_superiority_vs_coexistence,
    'Does the religious covenant frame logically foreclose secular political frameworks, or can they coexist as different domains (religious obligation for believers, secular negotiation for political action)?',
    'Examination of whether covenant-reading authorities accept secular frameworks as legitimate even if secondary — do they allow secular Jewish voices to negotiate, or do they suppress secular frameworks as incompatible? Analysis of institutional practice: does the state enforce covenant obligation through law, or does it permit secular dissent?',
    'If they can coexist, the constraint is tangled_rope (coordination + extraction through entanglement). If they logically foreclose each other, the constraint forecloses secular compromise space entirely and operates as snare (religious frame is cover for territorial extraction). This determines whether the suppression is structural or performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framework_superiority_vs_coexistence, conceptual, 'Whether religious and secular frameworks are logically independent or foreclosing.').

omega_variable(
    beneficiary_identity_fusion_ambiguity,
    'Are religious_zionist_establishment and settlement_movement actors genuinely identity-locked (their selves are constituted through covenant narrative) or strategically identity-locked (they claim identity fusion as a rhetorical tool to avoid negotiation)?',
    'Post-exit suppression trajectory: if settlement-movement members who leave continue to affirm the covenant obligation, the lock is internalized identity-fusion. If they reframe the covenant as political rather than constitutive and rebuild lives outside it, the lock is strategic performance. Analysis of dissenting voices within the movement.',
    'If genuinely fused, the constraint operates as identity_coordination type and the suppression reflects internalized identity barriers. If strategically locked, suppression is structural coercion and the constraint is snare masquerading as mountain. This affects the classification and the appropriate remediation (identity work vs. institutional change).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_identity_fusion_ambiguity, empirical, 'Whether identity fusion is constitutive or strategic.').

omega_variable(
    secular_framework_collapse_reversibility,
    'If the covenant reading''s political influence were removed, would secular compromise frameworks re-emerge as live options, or has their institutional suppression been sufficiently long that they cannot be restored?',
    'Comparative analysis: examine jurisdictions where religious authority has been institutionally reduced (Turkey, France post-secularization) to see whether secular frameworks re-emerge or whether institutional memory is too eroded. Examine historical periods when secular frameworks were dominant in Jewish/Israeli discourse.',
    'If secular frameworks can re-emerge, the suppression is structural and remediable. If institutional suppression has made them effectively extinct, the constraint has created irreversible asymmetry favoring the covenant frame. This affects whether the constraint can be resolved through framework adjustment or requires deeper institutional reconstruction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_framework_collapse_reversibility, empirical, 'Whether suppression of secular frameworks is reversible.').

omega_variable(
    mountain_vs_tangled_rope_terminal_attractor,
    'Is the religious covenant claim a genuine mountain (divine obligation that would persist regardless of political contestation) or a tangled_rope that only appears natural because religious authority has been institutionally entrenched?',
    'Comparative historical analysis: examine periods when Jewish communities lived without territorial sovereignty (diaspora, dispersion) and ask whether they experienced covenant obligation as binding on communal action or as theological claim that did not require territorial enforcement. Examine the relationship between institutional power and claim of naturalness: when religious authority is weaker, is the claim framed differently?',
    'If the covenant obligation genuinely commanded action in non-sovereign periods, it is an authentic mountain claim. If the political enforcement of territorial obligation is new (modern Zionism), the constraint is tangled_rope misclassified as mountain. This determines whether the false_summit_mountain signature is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_vs_tangled_rope_terminal_attractor, empirical, 'Whether the constraint is a genuinely natural obligation or institutionally enforced extraction.').

omega_variable(
    sibling_reading_mutual_foreclosure,
    'Does the religious_covenant_reading logically foreclose the settler_colonial_reading, or do they merely dispute the legitimacy of the same territorial claim?',
    'Logical analysis: the covenant reading asserts divine obligation makes the claim legitimate; the settler_colonial reading asserts the legitimacy is false (Zionism is colonialism regardless of framing). These are not about different facts — they are about the legitimacy frame itself. Can both be held in a single framework, or does one''s framework necessity exclude the other''s?',
    'If they foreclose each other, the constraint exhibits genuine logical foreclosure to a sibling reading. If they coexist as competing legitimacy claims about the same territorial arrangement, they are coexisting alternative readings (no foreclosure). This affects the cs_structure.reading_relations classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_mutual_foreclosure, conceptual, 'Whether covenant and settler-colonial readings are logically incompatible or competing frames on the same facts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__religious_covenant_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_self_determination__religious_covenant_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(jewi_tr_t10, jewish_self_determination__religious_covenant_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(jewi_tr_t25, jewish_self_determination__religious_covenant_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement(jewi_tr_t40, jewish_self_determination__religious_covenant_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(jewi_tr_t60, jewish_self_determination__religious_covenant_reading, theater_ratio, 60, 0.27).
narrative_ontology:measurement(jewi_tr_t75, jewish_self_determination__religious_covenant_reading, theater_ratio, 75, 0.28).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_self_determination__religious_covenant_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(jewi_be_t10, jewish_self_determination__religious_covenant_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(jewi_be_t25, jewish_self_determination__religious_covenant_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(jewi_be_t40, jewish_self_determination__religious_covenant_reading, base_extractiveness, 40, 0.76).
narrative_ontology:measurement(jewi_be_t60, jewish_self_determination__religious_covenant_reading, base_extractiveness, 60, 0.81).
narrative_ontology:measurement(jewi_be_t75, jewish_self_determination__religious_covenant_reading, base_extractiveness, 75, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_self_determination__religious_covenant_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(jewi_su_t10, jewish_self_determination__religious_covenant_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(jewi_su_t25, jewish_self_determination__religious_covenant_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(jewi_su_t40, jewish_self_determination__religious_covenant_reading, suppression_requirement, 40, 0.76).
narrative_ontology:measurement(jewi_su_t60, jewish_self_determination__religious_covenant_reading, suppression_requirement, 60, 0.78).
narrative_ontology:measurement(jewi_su_t75, jewish_self_determination__religious_covenant_reading, suppression_requirement, 75, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__religious_covenant_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__religious_covenant_reading, 0.12).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__diasporist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five readings of the jewish_self_determination kernel. The religious_covenant_reading asserts that Jewish territorial claims derive from divine obligation, independent of secular frameworks. The liberal_nationalist_reading grounds the same territorial claim in political philosophy of national self-determination. The indigenous_return_reading grounds it in historical connection. The settler_colonial_reading contests the legitimacy of the territorial claim itself. The diasporist_reading rejects territorial framing entirely. These five constraints are structurally linked: the covenant_reading forecloses secular compromise space (affects liberal_nationalist and diasporist), influences the indigenous_return reading's authority grounding, and is directly opposed by the settler_colonial reading. Each reading has distinct beneficiaries, victims, epsilon values, and institutional power distribution. The five readings form a constraint family linked by network.affects_constraints across all members.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_self_determination__religious_covenant_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
