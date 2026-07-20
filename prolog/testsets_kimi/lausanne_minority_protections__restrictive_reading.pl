% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__restrictive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__restrictive_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: lausanne_minority_protections__restrictive_reading
 *   human_readable: Lausanne Minority Protections â Restrictive Reading (Individual Worship Only)
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   The Lausanne Treaty (1923) contains minority protections that the Turkish
 *   state interprets restrictively: only individual worship is shielded,
 *   while institutional autonomy, property ownership, and theological
 *   education are treated as domestic matters subject to general Turkish law.
 *   This reading enables the state to confiscate minority community property,
 *   close seminaries (notably Halki), deny legal personality to religious
 *   foundations, and centralize control over non-Muslim institutional life.
 *   It is one reading of a contested kernel; sibling readings hold that
 *   Lausanne guarantees institutional continuity or internationally
 *   enforceable obligations.
 *
 * KEY AGENTS:
 *   - Turkish state apparatus: Agenda-setter and beneficiary (institutional/powerful exit) â consolidates sovereignty and captures institutional resources.
 *   - Non-Muslim minority institutions: Primary payer (organized/identity_locked) â bear expropriation and educational foreclosure.
 *   - Minority community members: Secondary payer (powerless/constrained) â bear cultural erasure costs.
 *   - Guarantor states: Excluded seat (institutional/constrained) â treaty parties structurally sidelined by domestication.
 *   - International human rights bodies: Observer (institutional/analytical) â monitor but cannot enforce.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__restrictive_reading, 0.82).
domain_priors:suppression_score(lausanne_minority_protections__restrictive_reading, 0.78).
domain_priors:theater_ratio(lausanne_minority_protections__restrictive_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__restrictive_reading, snare).
narrative_ontology:human_readable(lausanne_minority_protections__restrictive_reading, "Lausanne Minority Protections â Restrictive Reading (Individual Worship Only)").
narrative_ontology:topic_domain(lausanne_minority_protections__restrictive_reading, "international_law/religious_governance/minority_rights").

domain_priors:requires_active_enforcement(lausanne_minority_protections__restrictive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__restrictive_reading, '2a99060f-8922-4112-8860-2c87392832ab').
narrative_ontology:cs_kernel_codification('2a99060f-8922-4112-8860-2c87392832ab', fixed_text).
narrative_ontology:cs_authority_grounding('2a99060f-8922-4112-8860-2c87392832ab', extraction).
narrative_ontology:cs_interpretation_layer_present('2a99060f-8922-4112-8860-2c87392832ab').
narrative_ontology:cs_reading_relation('2a99060f-8922-4112-8860-2c87392832ab', lausanne_minority_protections__expansive_reading, forecloses).
narrative_ontology:cs_reading_relation('2a99060f-8922-4112-8860-2c87392832ab', lausanne_minority_protections__guarantor_reading, forecloses).
narrative_ontology:cs_axiom('2a99060f-8922-4112-8860-2c87392832ab', foundational, individual_worship_as_sole_protected_sphere).
narrative_ontology:cs_axiom_status(individual_worship_as_sole_protected_sphere, holdable).
narrative_ontology:cs_axiom_grounding('2a99060f-8922-4112-8860-2c87392832ab', individual_worship_as_sole_protected_sphere, conventional).
narrative_ontology:cs_axiom('2a99060f-8922-4112-8860-2c87392832ab', foundational, domestic_legal_supremacy_over_minority_institutions).
narrative_ontology:cs_axiom_status(domestic_legal_supremacy_over_minority_institutions, holdable).
narrative_ontology:cs_axiom_grounding('2a99060f-8922-4112-8860-2c87392832ab', domestic_legal_supremacy_over_minority_institutions, conventional).
narrative_ontology:cs_reference_frame('2a99060f-8922-4112-8860-2c87392832ab', sovereign_domestic_jurisdiction_framework).
narrative_ontology:cs_drift_state('2a99060f-8922-4112-8860-2c87392832ab', contemporary_human_rights_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2a99060f-8922-4112-8860-2c87392832ab', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, non_muslim_minority_institutions).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, minority_community_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the restrictive interpretation of the Lausanne Treaty through domestic legislation, court rulings, and executive policy. Confiscates minority property, closes theological schools, and denies legal personality to non-Muslim religious entities, consolidating sovereign control over all institutional religious life. Could change the interpretation by legislative or executive act but actively maintains the constraint.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus, beneficiary).

% Churches, foundations, and community boards stripped of legal personality and property rights under general Turkish law. Cannot operate theological seminaries or administer communal property independently. Exit is identity-locked: dissolving into the majority structure would mean cultural erasure, while maintaining their identity leaves them subject to expropriation and educational foreclosure.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, non_muslim_minority_institutions, payer,
    organized, generational, identity_locked, national).

% Individual worship is nominally protected but access to clergy formation, community property, and institutional continuity is eroded. Bear the cultural and material costs of the state's consolidation of minority institutional capacity. Emigration is possible but costly and severs community ties.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, minority_community_members, payer,
    powerless, biographical, constrained, national).

% Parties to the Lausanne Treaty who hold that minority protections include institutional continuity and are internationally enforceable. Structurally excluded from effective enforcement by the Turkish state's domestication of the treaty. Diplomatic and legal avenues are constrained by sovereignty norms and geopolitical considerations.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, guarantor_states, excluded,
    institutional, generational, constrained, global).

% Monitor and adjudicate complaints (e.g., ECtHR) regarding property restitution, religious freedom, and minority education. Lack enforcement capacity to compel state compliance with an expansive reading of the treaty. Their observations are analytical inputs rather than binding resolutions.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Delineates the boundary between international minority protection and domestic jurisdiction, restricting the former to individual religious practice while placing institutional governance, property, and education under ordinary state law.
% TRANSFER_FUNCTION: Moves institutional property, educational control, and legal personality from minority religious communities to the Turkish state apparatus, while transferring the costs of assimilated governance (closure of schools, seizure of properties, suppression of clergy training) onto minority institutions and their members.
% ABSENT_VOICES: Minority religious institutions that have been closed or expropriated, theological educators whose schools were abolished, and guarantor states party to Lausanne who hold that the treaty protects institutional continuity. Their exclusion is structural: the restrictive reading domesticates the treaty, removing international adjudication and minority institutional standing from the interpretive forum.
% DISAPPEARANCE_RATIONALE: If the restrictive reading vanished and the treaty's institutional protections were enforced, minority religious communities would regain legal personality, reclaimed property would shift back to community trusts, closed seminaries would reopen, and the state's monopoly over minority institutional formation would break. The domestic legal order would have to accommodate non-Muslim religious entities as autonomous juridical subjects.
% FOUNDING_PROBLEM: The post-Ottoman settlement needed to prevent irredentist minority nationalism and inter-communal conflict while securing Turkish territorial sovereignty; the Lausanne Treaty was drafted to balance minority survival guarantees with state sovereignty by protecting non-Muslim communities.
% FOUNDING_PROBLEM_CORROBORATION: Minority community historians and international legal scholars attest the founding problem was minority protection under new state sovereignty. The Turkish state apparatus attests the problem is sovereignty and public order; guarantor states and ECtHR jurisprudence from outside the benefiting party corroborate that the minority-protection function has been systematically eroded.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__restrictive_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__restrictive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__restrictive_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(lausanne_minority_protections__restrictive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__restrictive_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__restrictive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lausanne_minority_protections__restrictive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lausanne_minority_protections__restrictive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the constraint systematically transfers property and institutional capacity from minority communities to the state. Suppression is high (0.78) because the reading depends on actively blocking alternative interpretations, closing international enforcement channels, and denying legal personality. Theater ratio is moderate-high (0.50): the state periodically invokes Lausanne as evidence of minority protection while operating a structure that strips the very institutions the treaty was understood to protect. Resistance (0.60) reflects persistent minority legal challenges and intermittent international pressure. The measurement series track rising extraction across the treaty lifecycle, peaking in the EU candidacy period when theater was maximized, with continued enforcement consolidation in the contemporary period.
 *
 * PERSPECTIVAL GAP:
 *   From the Turkish state's seat, the arrangement is sovereign normalization: general law applied uniformly. From minority institutions' seats, it is targeted extraction that binds precisely to their identity. The engine computes this divergence from beneficiary/victim declarations and exit options â the state has mobile exit (can change the rule), while institutions are identity-locked.
 *
 * DIRECTIONALITY LOGIC:
 *   The Turkish state apparatus is the structural beneficiary (collects property and control, sets the rules â d near the beneficiary end). Minority institutions and community members are the structural targets (bear the costs of property seizure and educational foreclosure, with identity-locked or constrained exit â d near the target end). Guarantor states and international bodies are observers or excluded; their d values are analytically derived but do not receive extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The treaty originally coordinated a post-imperial minority-protection problem. The restrictive reading has outlived that coordination function: the founding problem of protecting non-Muslim communities under Turkish sovereignty has been transformed into a mechanism for state consolidation. The mandate is contested â the state claims the mandate is sovereignty maintenance, while minorities and guarantors claim the protective mandate is dead and the structure persists as extraction. This contested status is exactly what prevents automatic classification as either coordination or pure extraction without structural examination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lausanne_kernel_reading_ambiguity,
    'Is the Lausanne Treaty''s minority protection clause structurally limited to individual worship, or does it inherently guarantee institutional continuity including property, education, and legal personality?',
    'Comparative analysis of the treaty''s travaux prÃ©paratoires, subsequent state practice across successor regimes, and ECtHR adjudication on Article 9 and Article 1 Protocol 1 in minority contexts.',
    'If institutional guarantees are intrinsic, the restrictive reading is a snare using treaty language as cover for extraction; if individual worship is the sole protected sphere, the reading is a correct legal interpretation with low extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lausanne_kernel_reading_ambiguity, conceptual, 'Core ambiguity over whether Lausanne protects institutional or only individual religious rights.').

omega_variable(
    enforcement_locus_ambiguity,
    'Does the persistence of the restrictive reading depend primarily on domestic legal enforcement capacity, or on the international community''s strategic reluctance to activate guarantor state mechanisms?',
    'Tracing of diplomatic notes, ECtHR case admissibility patterns, and guarantor state joint demarches over the treaty lifecycle.',
    'If international inaction is the binding constraint, reclassification pressure should target guarantor mobilization; if domestic enforcement is the core mechanism, remedies must be domestic constitutional or legislative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_locus_ambiguity, empirical, 'Locus of enforcement: domestic state power vs international acquiescence.').

omega_variable(
    founding_problem_obsolescence,
    'Has the founding problem of minority protection under Turkish sovereignty been rendered obsolete by subsequent human rights regimes, or does the Lausanne framework remain structurally necessary?',
    'Assessment of whether ECHR and UN mechanisms have functionally superseded Lausanne''s minority-specific guarantees, or whether their non-discrimination framing leaves a protection gap that only Lausanne''s explicit minority provisions can fill.',
    'If obsolete, the constraint is a piton or snare maintained past its mandate; if still necessary but actively undermined, it is a captured snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, conceptual, 'Whether the founding minority-protection problem persists or has been superseded.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__restrictive_reading, 0, 95).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t0, lausanne_minority_protections__restrictive_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(laus_tr_t20, lausanne_minority_protections__restrictive_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(laus_tr_t40, lausanne_minority_protections__restrictive_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement(laus_tr_t60, lausanne_minority_protections__restrictive_reading, theater_ratio, 60, 0.55).
narrative_ontology:measurement(laus_tr_t80, lausanne_minority_protections__restrictive_reading, theater_ratio, 80, 0.65).
narrative_ontology:measurement(laus_tr_t95, lausanne_minority_protections__restrictive_reading, theater_ratio, 95, 0.5).

% Extraction over time
narrative_ontology:measurement(laus_be_t0, lausanne_minority_protections__restrictive_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(laus_be_t20, lausanne_minority_protections__restrictive_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(laus_be_t40, lausanne_minority_protections__restrictive_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(laus_be_t60, lausanne_minority_protections__restrictive_reading, base_extractiveness, 60, 0.72).
narrative_ontology:measurement(laus_be_t80, lausanne_minority_protections__restrictive_reading, base_extractiveness, 80, 0.78).
narrative_ontology:measurement(laus_be_t95, lausanne_minority_protections__restrictive_reading, base_extractiveness, 95, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(laus_su_t0, lausanne_minority_protections__restrictive_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(laus_su_t20, lausanne_minority_protections__restrictive_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(laus_su_t40, lausanne_minority_protections__restrictive_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(laus_su_t60, lausanne_minority_protections__restrictive_reading, suppression_requirement, 60, 0.75).
narrative_ontology:measurement(laus_su_t80, lausanne_minority_protections__restrictive_reading, suppression_requirement, 80, 0.7).
narrative_ontology:measurement(laus_su_t95, lausanne_minority_protections__restrictive_reading, suppression_requirement, 95, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections__expansive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections__guarantor_reading).

% DUAL FORMULATION NOTE:
% The Lausanne minority protections kernel decomposes into three structurally distinct constraints: the restrictive reading (individual worship only, domestic jurisdiction), the expansive reading (institutional continuity and autonomy), and the guarantor reading (international enforceability). They share the treaty text as kernel but instantiate different epsilon values and victim/beneficiary structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
