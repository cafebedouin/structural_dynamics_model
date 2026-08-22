% ============================================================================
% CONSTRAINT STORY: marriage_sacrament__hierarchical_indissolubility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_sacrament__hierarchical_indissolubility_reading, []).

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
 *   constraint_id: marriage_sacrament__hierarchical_indissolubility_reading
 *   human_readable: Marriage as Ontological Reality Requiring Hierarchical Adjudication; Indissolubility is Constitutive, Not Aspirational
 *   domain: religious_doctrine/canon_law/political_sociology
 *
 * SUMMARY:
 *   This constraint story instantiates the
 *   hierarchical_indissolubility_reading of the marriage_sacrament kernel. It
 *   treats marriage as an ontological reality — a sacrament that, once
 *   validly contracted, creates an indissoluble bond that no human power can
 *   dissolve. The hierarchy (pope, bishops, tribunals) holds exclusive
 *   adjudicative authority over whether a valid sacrament occurred (annulment
 *   = declaration that no valid sacrament existed). Divorced/remarried
 *   Catholics are excluded from Eucharist because their second union
 *   objectively contradicts the indissoluble first bond. The constraint
 *   extracts heavily from the divorced/remarried (spiritual exclusion, social
 *   marginalization) and from annulment petitioners (financial, temporal,
 *   emotional costs), while the clerical hierarchy and tribunal apparatus
 *   benefit through gatekeeping authority and institutional rationale.
 *   Suppression is high because the constraint actively denies sacraments and
 *   excludes dissenting pastoral voices. Theater is moderate: the tribunal
 *   process has genuine investigative function but increasingly serves to
 *   legitimate a predetermined ontological claim rather than discern truth.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__hierarchical_indissolubility_reading, 0.72).
domain_priors:suppression_score(marriage_sacrament__hierarchical_indissolubility_reading, 0.85).
domain_priors:theater_ratio(marriage_sacrament__hierarchical_indissolubility_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__hierarchical_indissolubility_reading, snare).
narrative_ontology:human_readable(marriage_sacrament__hierarchical_indissolubility_reading, "Marriage as Ontological Reality Requiring Hierarchical Adjudication; Indissolubility is Constitutive, Not Aspirational").
narrative_ontology:topic_domain(marriage_sacrament__hierarchical_indissolubility_reading, "religious_doctrine/canon_law/political_sociology").

domain_priors:requires_active_enforcement(marriage_sacrament__hierarchical_indissolubility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__hierarchical_indissolubility_reading, 'de17cdec-63ec-435d-b417-fa731735158f').
narrative_ontology:cs_kernel_codification('de17cdec-63ec-435d-b417-fa731735158f', formalized).
narrative_ontology:cs_authority_grounding('de17cdec-63ec-435d-b417-fa731735158f', lineage).
narrative_ontology:cs_interpretation_layer_present('de17cdec-63ec-435d-b417-fa731735158f').
narrative_ontology:cs_reading_relation('de17cdec-63ec-435d-b417-fa731735158f', marriage_sacrament__civic_pastoral_reading, coexists_with).
narrative_ontology:cs_axiom('de17cdec-63ec-435d-b417-fa731735158f', foundational, marriage_bond_ontologically_indissoluble).
narrative_ontology:cs_axiom_status(marriage_bond_ontologically_indissoluble, holdable).
narrative_ontology:cs_axiom_grounding('de17cdec-63ec-435d-b417-fa731735158f', marriage_bond_ontologically_indissoluble, deontological).
narrative_ontology:cs_axiom('de17cdec-63ec-435d-b417-fa731735158f', foundational, hierarchy_holds_exclusive_adjudicative_authority).
narrative_ontology:cs_axiom_status(hierarchy_holds_exclusive_adjudicative_authority, holdable).
narrative_ontology:cs_axiom_grounding('de17cdec-63ec-435d-b417-fa731735158f', hierarchy_holds_exclusive_adjudicative_authority, conventional).
narrative_ontology:cs_axiom('de17cdec-63ec-435d-b417-fa731735158f', secondary, eucharistic_coherence_requires_exclusion_of_remarried).
narrative_ontology:cs_axiom_status(eucharistic_coherence_requires_exclusion_of_remarried, holdable).
narrative_ontology:cs_axiom_grounding('de17cdec-63ec-435d-b417-fa731735158f', eucharistic_coherence_requires_exclusion_of_remarried, theological).
narrative_ontology:cs_reference_frame('de17cdec-63ec-435d-b417-fa731735158f', tridentine_sacramental_ontology).
narrative_ontology:cs_drift_state('de17cdec-63ec-435d-b417-fa731735158f', post_amoris_laetitia, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('de17cdec-63ec-435d-b417-fa731735158f', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__hierarchical_indissolubility_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, clerical_hierarchy).
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, canonical_tribunal_apparatus).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, divorced_remarried_catholics).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, catholics_denied_eucharist).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, annulment_petitioners).
narrative_ontology:constraint_vindicates(marriage_sacrament__hierarchical_indissolubility_reading, marriage_indissolubility_doctrine).
narrative_ontology:constraint_vindicates(marriage_sacrament__hierarchical_indissolubility_reading, hierarchical_ecclesiology).
narrative_ontology:constraint_vindicates(marriage_sacrament__hierarchical_indissolubility_reading, sacramental_ontology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets canon law on marriage, administers tribunals, controls access to sacraments. Claims authority from apostolic succession and magisterial teaching. Collects institutional legitimacy and sacramental gatekeeping power from maintaining indissolubility as ontological reality. Does not bear the costs of annulment processes or exclusion from Eucharist.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, clerical_hierarchy, agenda_setter,
    institutional, generational, arbitrage, global).

% Operates marriage tribunals processing annulment petitions. Collects fees, professional status, and institutional necessity from the adjudication function. Depends on the indissolubility constraint for its mandate; would lose institutional rationale if marriage were treated as dissoluble civil contract.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, canonical_tribunal_apparatus, beneficiary,
    organized, biographical, constrained, regional).

% Civilly divorced and remarried Catholics who remain Catholic in self-identification. Excluded from Eucharist and positions of ecclesial leadership. Bear the full extractive weight: spiritual exclusion, social stigma within parish communities, denial of sacramental grace as defined by the hierarchy. Exit requires either abandoning Catholic identity (identity_locked) or living in canonically irregular unions without recourse.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, divorced_remarried_catholics, payer,
    powerless, biographical, identity_locked, local).

% Broader group including divorced_remarried_catholics plus those in other irregular unions (e.g., cohabiting, same-sex civil marriages). Denied Eucharistic participation based on the same ontological claim. Identity_locked exit: leaving the Church means losing the sacramental universe that constitutes their spiritual self-understanding; staying means accepting exclusion.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, catholics_denied_eucharist, payer,
    powerless, biographical, identity_locked, local).

% Catholics seeking declaration of nullity for prior marriage. Bear financial costs (tribunal fees, canon lawyer costs), temporal costs (1-3 years typical), and emotional costs (intrusive investigation of marital intimacy). Exit options constrained: can abandon petition (losing any chance at regularization) or endure process. Some abandon Catholic practice rather than submit.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, annulment_petitioners, payer,
    moderate, immediate, constrained, regional).

% Parish priests, deacons, lay ministers who accompany divorced/remarried Catholics pastorally. Would advocate for communion access or streamlined processes but are bound by canonical obedience. Their exclusion from decision-making is structural — they implement but do not shape the constraint. Some quietly mitigate (e.g., not denying communion in practice) but risk canonical sanction.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, pastoral_caregivers, excluded,
    moderate, biographical, constrained, local).

% Catholic theologians and canonists who analyze the constraint from outside the governance structure. Produce the intellectual frameworks that either legitimate or critique the reading. Some (e.g., Rahner, Kasper) have proposed alternative readings; others defend the hierarchical_indissolubility_reading as dogmatically necessary. Their exit is analytical — they can change frameworks without losing institutional position (mostly).
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, theological_academy, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, objective criterion for sacramental marriage validity that prevents subjective dissolution and protects the sacramental order from becoming a mere human arrangement. Coordinates expectations across the global Church: a marriage validly contracted is indissoluble everywhere, by anyone's authority.
% TRANSFER_FUNCTION: Moves sacramental access (Eucharist, ecclesial office, canonical regularity) from divorced/remarried Catholics to the clerical hierarchy's gatekeeping authority. Moves financial resources (tribunal fees, canon lawyer costs) from annulment_petitioners to the canonical_tribunal_apparatus. Moves interpretive authority from pastoral caregivers to the magisterium.
% ABSENT_VOICES: Divorced/remarried Catholics who have left the Church entirely (exit realized but invisible to the constraint). Non-Catholic spouses in mixed marriages whose marriages are subject to canonical form requirements without their consent. Women disproportionately represented among annulment petitioners and denied communicants — gendered impact not represented in the all-male clerical hierarchy that adjudicates.
% DISAPPEARANCE_RATIONALE: If the hierarchical_indissolubility_reading vanished overnight, the canonical tribunal system would lose its primary caseload and institutional rationale; divorced/remarried Catholics would immediately regain Eucharistic access; pastoral caregivers would shift from gatekeeping to accompaniment; the global Catholic communion would reorganize around a pastoral_discernment model (the civic_pastoral_reading's framework). The constraint's disappearance would restructure the sacramental economy of the Church.
% FOUNDING_PROBLEM: Early Christian communities faced chaotic marriage practices: polygamy, easy divorce, uncertainty about validity of marriages contracted under pagan rites. The hierarchical_indissolubility_reading emerged to establish marriage as a sacrament reflecting Christ-Church union — ontologically indissoluble, not merely legally binding — requiring ecclesiastical adjudication to protect the sacramental sign from human caprice.
% FOUNDING_PROBLEM_CORROBORATION: The clerical hierarchy attests the founding problem remains live: secular divorce culture threatens the sacramental sign. Historical theologians (e.g., Gratian, Aquinas) corroborate the ontological claim's pedigree. But contemporary biblical scholars, pastoral theologians, and the civic_pastoral_reading's proponents attest the founding problem has shifted: the chaos was pre-modern; today's problem is the constraint's extraction from the faithful. No corroboration outside the benefiting parties for the claim that the original problem persists unchanged.
narrative_ontology:disappearance_verdict(marriage_sacrament__hierarchical_indissolubility_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_sacrament__hierarchical_indissolubility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__hierarchical_indissolubility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(marriage_sacrament__hierarchical_indissolubility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_sacrament__hierarchical_indissolubility_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_sacrament__hierarchical_indissolubility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_sacrament__hierarchical_indissolubility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_sacrament__hierarchical_indissolubility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) reflects the concentrated costs on divorced/remarried Catholics and annulment petitioners versus the diffuse institutional benefits to hierarchy/tribunals. Suppression (0.85) is very high because the constraint's persistence depends on active denial of sacraments, canonical penalties for non-compliance, and structural exclusion of alternative pastoral approaches. Theater (0.38) captures that tribunals do investigate but the outcome space is bounded by the indissolubility premise. Accessibility_collapse (0.78) is high because the ontological claim leaves no room for 'valid but dissolved' — alternatives collapse at the level of metaphysical possibility. Resistance (0.55) is moderate: significant pastoral resistance (Kasper proposal, Amoris Laetitia debates, German Synodal Way) but contained within institutional channels.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (clerical_hierarchy), the constraint appears as a Mountain: ontological reality, not human law. From payer seats (divorced_remarried_catholics, annulment_petitioners), it computes as Snare: active extraction with no exit. The engine computes this divergence from the declared power/exit/role structure. The claimed_type (snare) reflects the authoring seat's structural judgment; the engine will compute per-seat types from the same data.
 *
 * DIRECTIONALITY LOGIC:
 *   Clerical_hierarchy and canonical_tribunal_apparatus are structural beneficiaries (d ~ 0.1-0.2): they collect authority, legitimacy, institutional rationale. Divorced_remarried_catholics and catholics_denied_eucharist are full targets (d ~ 0.9-1.0): identity_locked exit, bear spiritual/social extraction. Annulment_petitioners are constrained payers (d ~ 0.7): they pay costs but have some procedural agency. Pastoral_caregivers are excluded (not beneficiaries, not payers directly, but structurally silenced). Theological_academy is analytical (d ~ 0.5): observes full structure without collecting or paying.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (chaotic pre-modern marriage practices) is largely solved by civil law and cultural stability in most societies. Yet the constraint persists with increased extraction (rising base_extractiveness from 0.45 to 0.72) and suppression. The tribunal apparatus has become self-justifying: its mandate (protect sacramental indissolubility) has atrophied into maintaining the adjudication machinery itself. Mandatrophy_resolved = false: the constraint's mandate has outlived its coordinating function but the hierarchy cannot acknowledge this without surrendering the ontological claim that grounds its authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_vs_constructed_indissolubility,
    'Is marriage indissolubility a genuine ontological feature of reality (as the reading claims) or a constructed doctrinal claim that benefits the clerical hierarchy''s adjudicative authority?',
    'Cross-cultural and historical comparison: if indissolubility appears as a human universal across religious traditions without hierarchical adjudication, it favors ontological reading. If it appears only where hierarchical clergy control sacramental gates, it favors constructed reading.',
    'If constructed, the constraint is a false summit (Mountain claim masking Snare operation) — FSM would trigger reclassification to tangled_rope or snare. If ontological, the Mountain claim holds and extraction is the cost of conforming to reality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ontological_vs_constructed_indissolubility, conceptual, 'Whether the constraint''s core premise is a natural law or a constructed claim with identifiable beneficiaries.').

omega_variable(
    annulment_as_coordination_or_extraction,
    'Does the annulment process genuinely coordinate truth-discovery about marital validity, or does it function as an extraction mechanism (fees, delays, intrusion) that legitimates the hierarchy''s gatekeeping?',
    'Compare annulment outcomes across tribunals: high variance suggests extraction/arbitrariness; high convergence suggests genuine discernment. Track petitioner outcomes: if most petitions succeed regardless of facts, process is theater; if denials correlate with evidence, process has coordination function.',
    'If extraction, the tribunal apparatus is a beneficiary of a snare/tangled_rope. If coordination, it is a rope/scaffold component. Affects classification of canonical_tribunal_apparatus stakeholder role and the constraint''s overall type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(annulment_as_coordination_or_extraction, empirical, 'Whether the adjudication machinery serves truth or power.').

omega_variable(
    identity_lock_mechanism,
    'What specific identity-fusion mechanism binds divorced_remarried_catholics to the constraint despite extraction? Is it sacramental ontology (the Eucharist IS their spiritual life), communal belonging (parish as primary social world), doctrinal internalization (they believe the hierarchy speaks for Christ), or fear of eternal consequence?',
    'Qualitative interviews with divorced/remarried Catholics who remain practicing vs. those who leave. Measure correlation between identity_lock strength and: frequency of communion desire, parish embeddedness, doctrinal literacy, eschatological anxiety.',
    'If identity_lock is primarily sacramental_ontology, the constraint''s extraction is inseparable from the good it guards (tangled_rope). If primarily communal_belonging or fear, the lock is social/psychological and could break under cultural shift — affecting exit_options and directionality over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Mechanism of identity-locked exit for primary victim group.').

omega_variable(
    reading_foreclosure_structure,
    'Does the hierarchical_indissolubility_reading logically foreclose the civic_pastoral_reading within a single Catholic framework, or do they coexist as competing legitimate positions?',
    'Analyze magisterial documents: if they declare the pastoral approach heretical/schismatic, foreclosure holds. If they tolerate it as legitimate theological opinion (even if not authoritative), coexistence holds. Track canonical penalties for priests practicing civic_pastoral approach.',
    'If forecloses, the kernel has a dominant reading that structurally excludes the sibling — reading_relations = forecloses. If coexists_with, both readings are live in different ecclesial factions. This determines cs_structure.reading_relations value and affects kernel-level drift analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_structure, conceptual, 'Structural relationship between the two kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__hierarchical_indissolubility_reading, 1917, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marriage_sacrament__hierarchical_indissolubility_reading_tr_t1917, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 1917, 0.25).
narrative_ontology:measurement(marriage_sacrament__hierarchical_indissolubility_reading_tr_t1940, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 1940, 0.28).
narrative_ontology:measurement(marriage_sacrament__hierarchical_indissolubility_reading_tr_t1965, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 1965, 0.3).
narrative_ontology:measurement(marriage_sacrament__hierarchical_indissolubility_reading_tr_t1983, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 1983, 0.35).
narrative_ontology:measurement(marriage_sacrament__hierarchical_indissolubility_reading_tr_t2000, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 2000, 0.36).
narrative_ontology:measurement(marriage_sacrament__hierarchical_indissolubility_reading_tr_t2015, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 2015, 0.37).
narrative_ontology:measurement(marriage_sacrament__hierarchical_indissolubility_reading_tr_t2025, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(marriage_sacrament__hierarchical_indissolubility_reading_be_t1917, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 1917, 0.45).
narrative_ontology:measurement(marriage_sacrament__hierarchical_indissolubility_reading_be_t1940, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 1940, 0.48).
narrative_ontology:measurement(marriage_sacrament__hierarchical_indissolubility_reading_be_t1965, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 1965, 0.52).
narrative_ontology:measurement(marriage_sacrament__hierarchical_indissolubility_reading_be_t1983, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 1983, 0.65).
narrative_ontology:measurement(marriage_sacrament__hierarchical_indissolubility_reading_be_t2000, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(marriage_sacrament__hierarchical_indissolubility_reading_be_t2015, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 2015, 0.7).
narrative_ontology:measurement(marriage_sacrament__hierarchical_indissolubility_reading_be_t2025, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(marriage_sacrament__hierarchical_indissolubility_reading_su_t1917, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 1917, 0.7).
narrative_ontology:measurement(marriage_sacrament__hierarchical_indissolubility_reading_su_t1940, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 1940, 0.72).
narrative_ontology:measurement(marriage_sacrament__hierarchical_indissolubility_reading_su_t1965, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 1965, 0.75).
narrative_ontology:measurement(marriage_sacrament__hierarchical_indissolubility_reading_su_t1983, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 1983, 0.82).
narrative_ontology:measurement(marriage_sacrament__hierarchical_indissolubility_reading_su_t2000, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 2000, 0.83).
narrative_ontology:measurement(marriage_sacrament__hierarchical_indissolubility_reading_su_t2015, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 2015, 0.84).
narrative_ontology:measurement(marriage_sacrament__hierarchical_indissolubility_reading_su_t2025, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 2025, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__hierarchical_indissolubility_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_sacrament__hierarchical_indissolubility_reading, 0.12).
narrative_ontology:affects_constraint(marriage_sacrament__hierarchical_indissolubility_reading, marriage_sacrament__civic_pastoral_reading).
narrative_ontology:affects_constraint(marriage_sacrament__hierarchical_indissolubility_reading, canonical_annulment_process).
narrative_ontology:affects_constraint(marriage_sacrament__hierarchical_indissolubility_reading, eucharistic_discipline).
narrative_ontology:affects_constraint(marriage_sacrament__hierarchical_indissolubility_reading, clerical_celibacy_discipline).

% DUAL FORMULATION NOTE:
% This constraint and civic_pastoral_reading form the marriage_sacrament kernel family. This reading (hierarchical_indissolubility) claims indissolubility as ontological constitutive; the sibling claims it as pastoral ideal. This reading's ε=0.72 reflects high extraction from divorced/remarried; the sibling's ε would be near 0.1 (pastoral accommodation). They share the same referent (Catholic marriage doctrine) but instantiate different constraints with different victim/beneficiary structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_sacrament__hierarchical_indissolubility_reading, institutional, 0.15).
constraint_indexing:directionality_override(marriage_sacrament__hierarchical_indissolubility_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
