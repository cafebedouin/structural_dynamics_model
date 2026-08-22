% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__restrictive_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__restrictive_sovereignty_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: refugee_convention_text__restrictive_sovereignty_reading
 *   human_readable: 1951 Refugee Convention — Restrictive Sovereignty Reading (Minimum Floor / Individualized Persecution)
 *   domain: international_law_migration_governance
 *
 * SUMMARY:
 *   This story authors the restrictive sovereignty reading of the 1951
 *   Refugee Convention / 1967 Protocol kernel: the Convention text is treated
 *   as a minimum floor of obligation that preserves maximum sovereign
 *   discretion over admission. Under this reading, 'well-founded fear'
 *   requires individualized proof of persecution directed at the specific
 *   claimant (generalized violence, civil war, and diffuse insecurity do not
 *   qualify), and 'particular social group' is narrowly construed to require
 *   an immutable or fundamental characteristic of which the state is aware —
 *   excluding more fluid, contested, or non-state-anchored group claims. This
 *   is one of three structurally distinct readings of the same treaty text
 *   (kernel: refugee_convention_text); the expansive_humanitarian_reading and
 *   procedural_integrity_reading are separate constraint stories with their
 *   own ε values and stakeholder sets, not alternative measurements of this
 *   one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, 0.68).
domain_priors:suppression_score(refugee_convention_text__restrictive_sovereignty_reading, 0.71).
domain_priors:theater_ratio(refugee_convention_text__restrictive_sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__restrictive_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__restrictive_sovereignty_reading, "1951 Refugee Convention — Restrictive Sovereignty Reading (Minimum Floor / Individualized Persecution)").
narrative_ontology:topic_domain(refugee_convention_text__restrictive_sovereignty_reading, "international_law_migration_governance").

domain_priors:requires_active_enforcement(refugee_convention_text__restrictive_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__restrictive_sovereignty_reading, 'b0e31632-1a0f-4f00-87e3-4b3d8b572064').
narrative_ontology:cs_kernel_codification('b0e31632-1a0f-4f00-87e3-4b3d8b572064', fixed_text).
narrative_ontology:cs_authority_grounding('b0e31632-1a0f-4f00-87e3-4b3d8b572064', lineage).
narrative_ontology:cs_interpretation_layer_present('b0e31632-1a0f-4f00-87e3-4b3d8b572064').
narrative_ontology:cs_reading_relation('b0e31632-1a0f-4f00-87e3-4b3d8b572064', refugee_convention_text__expansive_humanitarian_reading, forecloses).
narrative_ontology:cs_reading_relation('b0e31632-1a0f-4f00-87e3-4b3d8b572064', refugee_convention_text__procedural_integrity_reading, influences).
narrative_ontology:cs_axiom('b0e31632-1a0f-4f00-87e3-4b3d8b572064', foundational, sovereign_admission_discretion_primacy).
narrative_ontology:cs_axiom_status(sovereign_admission_discretion_primacy, holdable).
narrative_ontology:cs_axiom_grounding('b0e31632-1a0f-4f00-87e3-4b3d8b572064', sovereign_admission_discretion_primacy, conventional).
narrative_ontology:cs_axiom('b0e31632-1a0f-4f00-87e3-4b3d8b572064', foundational, persecution_requires_individualized_state_directed_proof).
narrative_ontology:cs_axiom_status(persecution_requires_individualized_state_directed_proof, holdable).
narrative_ontology:cs_axiom_grounding('b0e31632-1a0f-4f00-87e3-4b3d8b572064', persecution_requires_individualized_state_directed_proof, empirically_contingent).
narrative_ontology:cs_reference_frame('b0e31632-1a0f-4f00-87e3-4b3d8b572064', id_1951_postwar_state_consent_bargain).
narrative_ontology:cs_drift_state('b0e31632-1a0f-4f00-87e3-4b3d8b572064', post_1967_protocol_and_contemporary_displacement_patterns, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b0e31632-1a0f-4f00-87e3-4b3d8b572064', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, destination_state_governments).
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, border_enforcement_agencies).
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, offshore_processing_contractors).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_fleeing_generalized_violence).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, non_state_persecution_claimants).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, social_group_claimants_outside_immutability_test).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ratified the Convention but retain control over how 'well-founded fear' and 'particular social group' are construed domestically. Adopt the narrowest textually defensible reading — individualized persecution, immutable characteristics with state knowledge — to maximize the discretion to deny, deter, or externalize claims while remaining nominally compliant with treaty obligations.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, destination_state_governments, agenda_setter,
    institutional, generational, arbitrage, national).

% Operationalize the restrictive reading at the border and in status-determination interviews: apply high admissibility screening thresholds, require individualized evidentiary proof of persecution, and treat generalized-violence or non-state-actor claims as presumptively outside the definition. Their institutional workload and political mandate are served by a narrow definition that filters out most claims early.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, border_enforcement_agencies, beneficiary,
    institutional, immediate, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__restrictive_sovereignty_reading, border_enforcement_agencies, agenda_setter).

% Operate third-country processing facilities that destination states rely on precisely because the restrictive reading permits offshoring claims assessment outside domestic legal protections. Revenue and continued contracts depend on the sovereignty-maximizing interpretation remaining dominant.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, offshore_processing_contractors, beneficiary,
    organized, biographical, arbitrage, regional).

% Flee civil war, gang violence, or state collapse without being able to point to an individualized persecutory act directed at them personally. Under this reading their claims are categorically excluded from 'well-founded fear' regardless of the objective danger they face, leaving return, indefinite detention, or clandestine onward movement as the only options.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_fleeing_generalized_violence, payer,
    powerless, immediate, trapped, global).

% Face persecution from cartels, militias, domestic partners, or non-state armed groups. This reading requires proof the state either sanctioned the harm or is unable/unwilling to protect in a manner the adjudicator accepts as equivalent to state action — a high evidentiary bar that most cannot meet from outside the country, especially without legal representation.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, non_state_persecution_claimants, payer,
    powerless, immediate, trapped, global).

% Claim persecution on grounds — certain gender-based social positions, informal clan or caste affiliations, contested LGBTQ+ status where visibility is disputed — that adjudicators applying the immutable-characteristic-plus-state-awareness test treat as too fluid or too unknown-to-the-state to qualify as a 'particular social group,' regardless of the real danger involved.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, social_group_claimants_outside_immutability_test, payer,
    powerless, biographical, trapped, global).

% Publish interpretive guidelines urging broader readings of persecution and social group grounds, consistent with the object-and-purpose of the Convention, but have no binding enforcement power over sovereign courts and immigration agencies that adopt the restrictive textual reading instead. Their interpretive authority is persuasive only.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, unhcr_and_treaty_monitoring_bodies, excluded,
    organized, generational, constrained, global).

% Adjudicate appeals from denied claims, choosing between competing interpretive traditions. Some domestic courts defer heavily to the sovereignty-maximizing executive branch reading; others push back using humanitarian or procedural interpretive canons, creating inconsistent jurisprudence across jurisdictions applying the same treaty text.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, domestic_courts_reviewing_asylum_denials, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(refugee_convention_text__restrictive_sovereignty_reading, diffuse).
narrative_ontology:fixing_cost_class(refugee_convention_text__restrictive_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides states with a textually defensible, internationally recognized floor of obligation — the Convention's actual words — that permits coordinated, predictable, and legally defensible border and asylum administration without requiring states to accept broader protection categories they have not domestically legislated.
% TRANSFER_FUNCTION: Moves the burden of proof for danger from the state (which would otherwise bear responsibility for protecting anyone at risk) onto the individual claimant, who must produce individualized, often undocumentable evidence of persecution; this shifts the cost of the sovereignty-maximizing interpretation onto those least able to bear it — people fleeing violence with no legal representation, documentation, or safe access to evidence.
% ABSENT_VOICES: Asylum seekers whose claims are pre-screened out at admissibility or dismissed for failing the individualization or immutability tests are, by definition, not parties to the interpretive debate that excludes them; UNHCR's guidance function as persuasive commentary from outside the room where binding domestic interpretation is actually made.
% DISAPPEARANCE_RATIONALE: If this restrictive reading were displaced by the expansive humanitarian reading, protection would extend to millions currently excluded for lacking individualized persecution evidence or falling outside the immutability test — asylum systems, detention capacity, and offshore processing arrangements would need to be rebuilt around a materially larger recognized refugee population.
% FOUNDING_PROBLEM: The 1951 Convention was drafted to give post-WWII European displaced persons a durable, internationally recognized legal status while allowing signatory states — still rebuilding and wary of open-ended obligations — to retain control over admission numbers and criteria; the restrictive reading traces its lineage to that original bargain between humanitarian aim and sovereign consent.
% FOUNDING_PROBLEM_CORROBORATION: States and their border agencies attest the sovereignty-preserving founding bargain remains live and textually grounded. UNHCR, refugee law scholars, and dissenting domestic judges — parties outside the states benefiting from the restrictive reading — attest that the drafters' 1951 European-refugee-specific compromise was substantially broadened by the 1967 Protocol's removal of geographic and temporal limits, and that treating the original narrow bargain as still controlling ignores that expansion; this corroboration comes from outside the beneficiary set.
narrative_ontology:disappearance_verdict(refugee_convention_text__restrictive_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__restrictive_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__restrictive_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(refugee_convention_text__restrictive_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__restrictive_sovereignty_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68) and suppression (0.71) are both substantial and rising because the restrictive reading's operational effect over 70+ years has been to filter an increasing share of asylum claims out of the protected category as global displacement has shifted from state-directed individualized persecution (the paradigm the drafters had most clearly in view) toward generalized violence, civil conflict, and non-state persecution. Theater ratio (0.42) reflects that admissibility screening increasingly performs rigorous individualized assessment while the categorical exclusions built into the reading's definitions predetermine much of the outcome before evidence is weighed.
 *
 * PERSPECTIVAL GAP:
 *   From the state agenda-setter seat, this reading is principled treaty fidelity — applying the actual negotiated text rather than an aspirational gloss. From the payer seats (claimants excluded by the individualization and immutability tests), the identical textual provisions operate as an enforced exclusion mechanism that requires active screening infrastructure, detention capacity, and offshore contracting to maintain. The engine's per-seat computation is expected to diverge sharply between these positions; the divergence is the finding, not an error in either seat's classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Destination-state governments and their enforcement agencies are the structural beneficiaries: the reading maximizes their discretion to deny, detain, or externalize claims while remaining textually compliant. Offshore processing contractors benefit derivatively from the discretion this reading legitimizes. Claimants fleeing generalized violence, non-state persecution, or asserting social-group grounds outside the immutability test are the structural targets — trapped, powerless, and bearing an evidentiary burden the reading's own categories make close to impossible to satisfy from outside safe legal process.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (giving durable status to displaced persons while respecting fragile postwar state capacity) was substantially addressed by the 1960s and materially altered by the 1967 Protocol's removal of the original geographic and temporal limits. The restrictive reading's persistence past that expansion — continuing to invoke a narrow, sovereignty-preserving bargain as though the Protocol had not broadened the mandate — is exactly the founding-problem/disappearance mismatch this framework is built to surface: status is authored as 'contested' rather than resolved, and the corroboration comes from outside the beneficiary set (UNHCR, scholars, dissenting courts), which is what keeps this from being a self-serving genealogy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    restrictive_reading_textual_fidelity_vs_purpose_evasion,
    'Is the restrictive sovereignty reading a faithful, textualist application of the Convention''s actual negotiated language, or a purpose-evading construction that exploits interpretive latitude to reintroduce the sovereign discretion the treaty was drafted to constrain?',
    'Comparative analysis of travaux préparatoires against post-1967-Protocol state practice; examination of whether domestic courts applying this reading cite the Protocol''s expansion or treat the 1951 negotiating context as still controlling.',
    'If textually faithful, the reading is defensible treaty interpretation with genuine coordination value (predictable, consistent, legally grounded admission control). If purpose-evading, the coordination story is substantially cover for extraction — a tangled_rope classification with the coordination function weaker than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restrictive_reading_textual_fidelity_vs_purpose_evasion, conceptual, 'Whether the restrictive reading is genuine textualism or purpose-evasion dressed as textualism.').

omega_variable(
    kernel_reading_selection_mechanism,
    'Why does this reading dominate in some domestic legal systems and not others — is reading-selection driven by genuine interpretive disagreement, or by which reading serves the adjudicating state''s immigration-control interests?',
    'Cross-jurisdictional comparison of reading adoption against measures of net migration pressure and domestic political salience of immigration restriction.',
    'If selection tracks state self-interest rather than principled interpretation, this strengthens the case that the kernel contest itself is partly endogenous to the extraction the restrictive reading produces.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_mechanism, empirical, 'Whether reading-adoption across states tracks interpretive principle or self-interested convenience.').

omega_variable(
    immutability_test_natural_kind_or_construct,
    'Is the immutable-characteristic-plus-state-awareness test for ''particular social group'' a natural boundary the treaty text compels, or a constructed limit that could be read more broadly without doing violence to the text?',
    'Comparative doctrinal analysis of how courts applying the expansive reading construe the identical treaty phrase ''particular social group'' to include gender and LGBTQ+ status without additional textual amendment.',
    'If the same words support a materially broader construction elsewhere, the restrictive reading''s narrowness is a policy choice, not a textual necessity, and its extraction is less attributable to Convention constraint and more to interpretive choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immutability_test_natural_kind_or_construct, conceptual, 'Whether the narrow social-group test is textually compelled or a discretionary interpretive choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__restrictive_sovereignty_reading, 1951, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t1951, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 1951, 0.2).
narrative_ontology:measurement(refu_tr_t1970, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 1970, 0.24).
narrative_ontology:measurement(refu_tr_t1990, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 1990, 0.28).
narrative_ontology:measurement(refu_tr_t2001, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 2001, 0.33).
narrative_ontology:measurement(refu_tr_t2015, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(refu_tr_t2024, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(refu_be_t1951, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 1951, 0.35).
narrative_ontology:measurement(refu_be_t1970, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 1970, 0.42).
narrative_ontology:measurement(refu_be_t1990, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(refu_be_t2001, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 2001, 0.58).
narrative_ontology:measurement(refu_be_t2015, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 2015, 0.63).
narrative_ontology:measurement(refu_be_t2024, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t1951, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 1951, 0.4).
narrative_ontology:measurement(refu_su_t1970, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 1970, 0.48).
narrative_ontology:measurement(refu_su_t1990, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(refu_su_t2001, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 2001, 0.62).
narrative_ontology:measurement(refu_su_t2015, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 2015, 0.68).
narrative_ontology:measurement(refu_su_t2024, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 2024, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__restrictive_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, expansive_humanitarian_reading).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, procedural_integrity_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories decompose the single natural-language label 'the 1951 Refugee Convention' per the ε-invariance principle: restrictive_sovereignty_reading (this story, ε=0.68, tangled_rope), expansive_humanitarian_reading (higher coordination emphasis, narrower extraction as authored from that reading's own lights), and procedural_integrity_reading (extraction concentrated in process failures rather than substantive exclusion). All three share the refugee_convention_text kernel but produce different victim sets, different ε, and different classifications because they read the same treaty language differently. Linked bidirectionally via affects_constraints; each reading's shift in dominant judicial or state adoption structurally pressures the others' operative reach.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
