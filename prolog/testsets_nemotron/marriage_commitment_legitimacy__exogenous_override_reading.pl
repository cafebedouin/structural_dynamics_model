% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__exogenous_override_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__exogenous_override_reading
 *   human_readable: Federal Coercion Override of LDS Marriage Legitimacy (1890 Manifesto)
 *   domain: religious_institutional_history/political_theology/commitment_systems
 *
 * SUMMARY:
 *   The 1890 Manifesto (Official Declaration 1) publicly suspended the LDS
 *   practice of plural marriage under sustained federal coercion: the Edmunds
 *   Act (1882), Edmunds-Tucker Act (1887), and threat of total corporate
 *   dissolution and asset seizure. This reading holds that the Manifesto was
 *   not a genuine prophetic revelation but a forced capitulation — federal
 *   power extracted institutional compliance while theological doctrine
 *   (plural marriage as an eternal, celestial principle) remained formally
 *   unchanged. The constraint is the standing arrangement: the Church's
 *   legitimacy now rests on a suspended practice whose doctrine persists,
 *   creating a structural gap between what members are taught is eternally
 *   true and what they are permitted to practice. Federal sovereignty is the
 *   beneficiary; LDS members (especially polygamous families) are the victims
 *   bearing the costs of doctrinal abandonment, social stigma, and the
 *   legitimacy crisis of a prophetic office that appears to have yielded to
 *   coercion.
 *
 * KEY AGENTS:
 *   - federal_government: Primary beneficiary (institutional/biographical) — extracts institutional submission to federal sovereignty
 *   - lds_membership: Primary victim (organized/biographical) — bears doctrinal abandonment costs and legitimacy crisis
 *   - polygamous_families: Secondary victim (powerless/biographical) — bear direct practice suppression, property loss, disenfranchisement
 *   - lds_leadership: Dual-positioned agenda_setter/payer (institutional/biographical) — administers the constraint while bearing institutional survival costs
 *   - fundamentalist_schismatics: Excluded (powerless/biographical) — reject the constraint entirely, maintain practice at high cost
 *   - analytical_observer: Observer (analytical/civilizational) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__exogenous_override_reading, 0.82).
domain_priors:suppression_score(marriage_commitment_legitimacy__exogenous_override_reading, 0.91).
domain_priors:theater_ratio(marriage_commitment_legitimacy__exogenous_override_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__exogenous_override_reading, snare).
narrative_ontology:human_readable(marriage_commitment_legitimacy__exogenous_override_reading, "Federal Coercion Override of LDS Marriage Legitimacy (1890 Manifesto)").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__exogenous_override_reading, "religious_institutional_history/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__exogenous_override_reading, 'a21563ef-77af-4a9c-b3e5-ca311d5ca398').
narrative_ontology:cs_kernel_codification('a21563ef-77af-4a9c-b3e5-ca311d5ca398', formalized).
narrative_ontology:cs_authority_grounding('a21563ef-77af-4a9c-b3e5-ca311d5ca398', lineage).
narrative_ontology:cs_interpretation_layer_present('a21563ef-77af-4a9c-b3e5-ca311d5ca398').
narrative_ontology:cs_reading_relation('a21563ef-77af-4a9c-b3e5-ca311d5ca398', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('a21563ef-77af-4a9c-b3e5-ca311d5ca398', marriage_commitment_legitimacy__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_axiom('a21563ef-77af-4a9c-b3e5-ca311d5ca398', foundational, manifesto_was_coerced_capitulation).
narrative_ontology:cs_axiom_status(manifesto_was_coerced_capitulation, holdable).
narrative_ontology:cs_axiom_grounding('a21563ef-77af-4a9c-b3e5-ca311d5ca398', manifesto_was_coerced_capitulation, empirically_contingent).
narrative_ontology:cs_axiom('a21563ef-77af-4a9c-b3e5-ca311d5ca398', foundational, plural_marriage_doctrine_remains_eternal).
narrative_ontology:cs_axiom_status(plural_marriage_doctrine_remains_eternal, holdable).
narrative_ontology:cs_axiom_grounding('a21563ef-77af-4a9c-b3e5-ca311d5ca398', plural_marriage_doctrine_remains_eternal, deontological).
narrative_ontology:cs_reference_frame('a21563ef-77af-4a9c-b3e5-ca311d5ca398', prophetic_authority_intact_pre_1890).
narrative_ontology:cs_drift_state('a21563ef-77af-4a9c-b3e5-ca311d5ca398', post_manifesto_crisis_1890_1904, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('a21563ef-77af-4a9c-b3e5-ca311d5ca398', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, federal_government).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, lds_membership).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, polygamous_families).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, lds_leadership).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__exogenous_override_reading, federal_sovereignty_over_territorial_governance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Applied sustained legislative, judicial, and executive pressure (Edmunds Act, Edmunds-Tucker Act, Reynolds v. US) to force LDS institutional capitulation on plural marriage. Gained territorial control, eliminated rival theocratic sovereignty claim, established federal supremacy over religious corporations. Cost of enforcement was absorbed in ordinary governance; benefits were structural and durable.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, federal_government, beneficiary,
    institutional, generational, arbitrage, national).

% Bound by covenant, community, and cosmology to the prophetic office. The Manifesto created a structural contradiction: the prophet declared God's eternal law (plural marriage) but suspended its practice under threat. Members bear the cost of maintaining faith in prophetic authority while accepting the suspension. Exit means abandoning eternal family sealings, community, and identity — structurally identity_locked. The legitimacy crisis persists across generations.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, lds_membership, payer,
    organized, biographical, identity_locked, national).

% Direct targets of federal prosecution: property seizure (>$50k escheat), disenfranchisement, imprisonment of husbands, bastardization of children. No effective exit — kin networks, property, and spiritual commitments bind them. The Manifesto offered no provision for existing families; they were abandoned by both the federal government (no grandfathering) and the Church (public disavowal).
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, polygamous_families, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__exogenous_override_reading, polygamous_families, excluded).

% Administered the capitulation: issued the Manifesto, testified before Congress, managed the transition to monogamous public face while preserving temple doctrine. As agenda_setters they coordinate institutional survival (d ~0.15); as payers they bear the cost of managing the legitimacy fracture, developing doctrinal frameworks to sustain the doctrine/practice separation, and suppressing fundamentalist schisms (d ~0.85). Their exit is constrained — they cannot abandon the institution they lead.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, lds_leadership, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__exogenous_override_reading, lds_leadership, payer).

% Reject the Manifesto entirely as illegitimate; continue plural marriage practice at extreme cost (prosecution, exile, social marginalization). They are excluded from the institutional conversation — their objection is structurally silenced by both the federal government (criminalization) and the LDS Church (excommunication). Their identity is fused to the rejected practice; exit from their position means abandoning what they hold as eternal truth.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, fundamentalist_schismatics, excluded,
    powerless, biographical, identity_locked, local).

% Observes the full structural field: federal extraction of institutional submission, LDS identity-locked bearing of doctrinal abandonment, leadership's dual coordination/extraction position, fundamentalist exclusion. No material stake; computes classification from structural data.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates federal territorial governance by eliminating a rival theocratic sovereignty claim; coordinates LDS institutional survival by providing a face-saving capitulation mechanism.
% TRANSFER_FUNCTION: Moves institutional sovereignty and doctrinal coherence from the LDS Church to the federal government, in exchange for the Church's legal personality, property, and path to statehood. The transfer is asymmetrical: federal government gains structural supremacy; LDS Church gains survival at cost of legitimacy fracture.
% ABSENT_VOICES: Polygamous families (especially women and children) had no representative voice in the Manifesto negotiations. Fundamentalist schismatics are permanently excluded from institutional discourse. The federal government's broader objective (institutional submission vs. practice regulation) was never explicitly negotiated — it was imposed.
% DISAPPEARANCE_RATIONALE: If the Manifesto and its enforcement vanished overnight, the LDS Church would face immediate re-litigation of its corporate status, property claims, and the theological status of plural marriage. The federal government would lose its established precedent for regulating religious corporations. Polygamous families would have a legal opening for property restoration. The entire Utah statehood compact would be destabilized.
% FOUNDING_PROBLEM: Federal territorial governance required elimination of LDS theocratic sovereignty; LDS institutional survival required a mechanism to comply without admitting doctrinal error.
% FOUNDING_PROBLEM_CORROBORATION: Utah statehood (1896) resolved the federal territorial governance problem — corroborated by federal congressional record and Utah admission documents. The LDS Church's survival is corroborated by its continued existence. However, the doctrinal coherence problem persists: LDS leadership's own doctrinal maintenance efforts (temple liturgy, prophetic statements, fundamentalist suppression) corroborate that the founding problem's second half (survival without doctrinal error) remains unresolved — no external corroboration exists for successful resolution of the legitimacy fracture.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__exogenous_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(marriage_commitment_legitimacy__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__exogenous_override_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_legitimacy__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.82 at 1890) because the federal government extracted not just practice cessation but institutional surrender: corporate dissolution was the leverage, and the Church's legal personality was the price. Suppression is extreme (0.91) because alternatives (continued practice, territorial statehood on LDS terms, religious freedom claims) were actively destroyed by federal enforcement machinery. Theater is low (0.18) initially because the coercion was overt — the Manifesto was a public capitulation document, not a performance. Over time, theater rises as the Church develops elaborate doctrinal frameworks to maintain the doctrine/practice separation (0.68 by 2000). Accessibility collapse is moderate (0.62) because the constraint is specific to LDS institutional context — members could theoretically exit, but identity-locked exit makes alternatives psychologically inaccessible. Resistance is high (0.74) because fundamentalist schisms, underground practice, and persistent doctrinal tension demonstrate active refusal to accept the constraint as legitimate.
 *
 * PERSPECTIVAL GAP:
 *   From the federal seat, the constraint appears as legitimate regulation of a harmful practice (rope-like coordination). From the LDS member seat, it appears as coerced betrayal of eternal doctrine (snare). From the leadership seat, it appears as tragic necessity for institutional survival (tangled_rope — coordination of survival via extraction of doctrinal coherence). The engine computes these divergences from the structural data; the claim (snare) reflects the analytical observer's assessment that extraction dominates.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government is the structural beneficiary (d ≈ 0.05): it gained territorial control, eliminated a rival sovereignty claim, and established federal supremacy over religious corporations — all at near-zero cost. LDS membership is the primary target (d ≈ 0.95): identity-locked into the institution, they bear the full cost of doctrinal incoherence, social stigma, and the legitimacy crisis of sustaining a prophetic office that appeared to capitulate. Polygamous families are hyper-targets (d ≈ 0.98): trapped by kin networks, property seizure, and disenfranchisement. LDS leadership sits in a dual position: as agenda_setter they administer the constraint (d ≈ 0.15 for the coordination function of institutional survival), but as payers they bear the cost of managing the legitimacy fracture (d ≈ 0.85 for the extraction function). Fundamentalist schismatics are excluded — their exit is structurally trapped (identity-locked into the rejected practice), but they reject the constraint entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal territorial governance vs. LDS theocratic claim) was largely resolved by 1896 statehood, but the constraint persisted because the doctrine/practice gap became a stable extraction mechanism: the Church could claim prophetic continuity while complying with federal law, and the federal government could claim regulatory victory while tolerating doctrinal persistence. This mutual face-saving arrangement converted a snare into a piton over time — the theater ratio rise documents this. The mandate (institutional survival) outlived its crisis function, but the constraint persists as identity-maintenance theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrine_practice_separation_viability,
    'Can a theological doctrine remain unchanged while its mandated practice is suspended under duress without the doctrine itself being functionally altered?',
    'Track doctrinal discourse after 1890: if prophetic statements, temple liturgy, and member catechesis continue to affirm plural marriage as an eternal principle while forbidding its current practice, the separation is structurally maintained; if the doctrine is quietly rewritten or deprecated, the separation collapses.',
    'If the separation holds, the constraint is pure extraction (snare) with doctrine as hostage; if the separation collapses, the constraint becomes a tangled_rope where the institution coordinates its own survival by rewriting its kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_practice_separation_viability, conceptual, 'Whether the doctrine/practice distinction survives coercive suspension').

omega_variable(
    federal_intent_extraction_vs_regulation,
    'Was the federal government''s objective the regulation of a specific practice (polygamy) or the extraction of institutional submission to federal sovereignty?',
    'Compare the Edmunds-Tucker Act''s provisions (corporate dissolution, escheat of property > $50k, disenfranchisement of polygamists, elimination of LDS church legal personality) against the stated goal of ''suppressing polygamy''; examine whether compliance on polygamy alone would have satisfied the Acts or whether corporate surrender was the actual condition for relief.',
    'If submission was the objective, the constraint is a snare with federal sovereignty as beneficiary; if practice suppression was the sole objective, the constraint has a genuine regulatory function tangled with extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federal_intent_extraction_vs_regulation, empirical, 'Whether federal coercion targeted practice or institutional legitimacy itself').

omega_variable(
    kernel_reading_underdetermination,
    'Does the ''exogenous override'' reading of the Manifesto foreclose the ''endogenous revelation'' reading, or do both remain structurally coherent within different commitment frameworks?',
    'Test whether a single agent can simultaneously hold: (a) the Manifesto was coerced capitulation AND (b) the Manifesto was genuine prophetic revelation, without logical contradiction. If no single framework can hold both, the readings are mutually foreclosing; if different parties can hold each without internal contradiction, they coexist.',
    'If forecloses, the kernel has a genuine schism; if coexists_with, the kernel sustains multiple live readings and the constraint''s classification varies by reading seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Structural relationship between the exogenous_override_reading and endogenous_reinterpretation_reading of the marriage_commitment_legitimacy kernel').

omega_variable(
    member_legitimacy_crisis_trajectory,
    'Did the legitimacy crisis among LDS members resolve into a new stable equilibrium, or does it persist as an unresolved fracture in the commitment system?',
    'Measure doctrinal discourse, temple recommend interview questions, and member exit/voice patterns across generations (1890-1920, 1920-1950, 1950-present). Persistent underground plural marriage communities, continued prophetic ambiguity, and recurring ''fundamentalist'' schisms indicate unresolved fracture.',
    'If unresolved, the constraint''s extraction continues via legitimacy costs borne by members; if resolved, the constraint transitions toward piton (inertial persistence of a resolved arrangement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(member_legitimacy_crisis_trajectory, empirical, 'Whether the member legitimacy crisis persists as ongoing extraction or resolved into new equilibrium').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__exogenous_override_reading, 1890, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1890, 0.18).
narrative_ontology:measurement(marr_tr_t1904, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1904, 0.22).
narrative_ontology:measurement(marr_tr_t1920, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1920, 0.35).
narrative_ontology:measurement(marr_tr_t1950, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1950, 0.48).
narrative_ontology:measurement(marr_tr_t1978, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1978, 0.61).
narrative_ontology:measurement(marr_tr_t2000, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 2000, 0.68).

% Extraction over time
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1890, 0.82).
narrative_ontology:measurement(marr_be_t1904, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1904, 0.78).
narrative_ontology:measurement(marr_be_t1920, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1920, 0.65).
narrative_ontology:measurement(marr_be_t1950, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1950, 0.42).
narrative_ontology:measurement(marr_be_t1978, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1978, 0.31).
narrative_ontology:measurement(marr_be_t2000, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 2000, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1890, 0.91).
narrative_ontology:measurement(marr_su_t1904, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1904, 0.88).
narrative_ontology:measurement(marr_su_t1920, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1920, 0.72).
narrative_ontology:measurement(marr_su_t1950, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1950, 0.45).
narrative_ontology:measurement(marr_su_t1978, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1978, 0.28).
narrative_ontology:measurement(marr_su_t2000, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 2000, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__exogenous_override_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_commitment_legitimacy__exogenous_override_reading, 0.08).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__exogenous_override_reading, lds_institutional_legitimacy_post_1890).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__exogenous_override_reading, federal_territorial_governance_utah).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__exogenous_override_reading, religious_freedom_jurisprudence_reynolds_v_us).

% DUAL FORMULATION NOTE:
% Part of the marriage_commitment_legitimacy constraint family. This reading (exogenous_override) has high ε (0.82) because it centers federal extraction. The endogenous_reinterpretation_reading would have low ε (genuine revelation = coordination). The hybrid_pragmatic_reading would have moderate ε (strategic adaptation = tangled_rope). The three readings share the kernel but instantiate different constraints with different ε, beneficiaries, and victims — per ε-invariance principle, they are separate stories linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_legitimacy__exogenous_override_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
