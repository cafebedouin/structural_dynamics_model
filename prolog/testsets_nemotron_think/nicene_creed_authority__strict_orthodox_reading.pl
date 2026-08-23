% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__strict_orthodox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__strict_orthodox_reading, []).

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
 *   constraint_id: nicene_creed_authority__strict_orthodox_reading
 *   human_readable: Nicene Creed Authority (Strict Orthodox Reading)
 *   domain: systematic_theology/ecclesiology/history_of_christian_doctrine
 *
 * SUMMARY:
 *   The strict orthodox reading of the Nicene Creed asserts that the creed's
 *   metaphysical claims (homoousios, Trinitarian formula, Christological
 *   definitions) are binding on all believers as divinely revealed truth.
 *   Deviation is not merely error but heresy, warranting ecclesiastical
 *   sanctions (excommunication, anathema) and historically civil penalties.
 *   The constraint operates through a hierarchical teaching office
 *   (magisterium) that claims authority from apostolic succession and
 *   ecumenical councils. The reading presents the creed as the essential
 *   coordination mechanism for Christian unity, but its enforcement history
 *   reveals substantial extraction: the clergy's interpretive monopoly, the
 *   suppression of alternative metaphysical frameworks, and the
 *   identity-locking of lay believers.
 *
 * KEY AGENTS:
 *   - hierarchical_clergy: Primary beneficiary and agenda_setter (institutional/analytical) — defines orthodoxy, collects authority rents
 *   - heterodox_communities: Primary victim/payer (organized/constrained) — bears sanctions, loses standing
 *   - lay_interpreters: Victim/payer (moderate/identity_locked) — faces spiritual/social sanctions, exit blocked by identity fusion
 *   - ecclesiastical_courts: Agenda_setter (institutional/analytical) — administers enforcement machinery
 *   - orthodox_laity: Beneficiary (moderate/mobile) — gains communal identity and sacramental assurance
 *   - secular_authorities: Observer (powerful/analytical) — historically co-opted enforcement for political order
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__strict_orthodox_reading, 0.65).
domain_priors:suppression_score(nicene_creed_authority__strict_orthodox_reading, 0.75).
domain_priors:theater_ratio(nicene_creed_authority__strict_orthodox_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__strict_orthodox_reading, tangled_rope).
narrative_ontology:human_readable(nicene_creed_authority__strict_orthodox_reading, "Nicene Creed Authority (Strict Orthodox Reading)").
narrative_ontology:topic_domain(nicene_creed_authority__strict_orthodox_reading, "systematic_theology/ecclesiology/history_of_christian_doctrine").

domain_priors:requires_active_enforcement(nicene_creed_authority__strict_orthodox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__strict_orthodox_reading, '1d564578-44b9-4ae5-ad97-1318ececd509').
narrative_ontology:cs_kernel_codification('1d564578-44b9-4ae5-ad97-1318ececd509', fixed_text).
narrative_ontology:cs_authority_grounding('1d564578-44b9-4ae5-ad97-1318ececd509', lineage).
narrative_ontology:cs_interpretation_layer_present('1d564578-44b9-4ae5-ad97-1318ececd509').
narrative_ontology:cs_reading_relation('1d564578-44b9-4ae5-ad97-1318ececd509', nicene_creed_authority__symbolic_confessional_reading, coexists_with).
narrative_ontology:cs_reading_relation('1d564578-44b9-4ae5-ad97-1318ececd509', nicene_creed_authority__liturgical_habituation_reading, coexists_with).
narrative_ontology:cs_axiom('1d564578-44b9-4ae5-ad97-1318ececd509', foundational, creed_metaphysically_binding).
narrative_ontology:cs_axiom_status(creed_metaphysically_binding, holdable).
narrative_ontology:cs_axiom_grounding('1d564578-44b9-4ae5-ad97-1318ececd509', creed_metaphysically_binding, deontological).
narrative_ontology:cs_axiom('1d564578-44b9-4ae5-ad97-1318ececd509', foundational, heresy_sanction_legitimate).
narrative_ontology:cs_axiom_status(heresy_sanction_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('1d564578-44b9-4ae5-ad97-1318ececd509', heresy_sanction_legitimate, deontological).
narrative_ontology:cs_reference_frame('1d564578-44b9-4ae5-ad97-1318ececd509', patristic_conciliar_framework).
narrative_ontology:cs_drift_state('1d564578-44b9-4ae5-ad97-1318ececd509', modern_critical_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1d564578-44b9-4ae5-ad97-1318ececd509', '2026-08-15T12:00:00Z').
narrative_ontology:cs_kernel_id(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, heterodox_communities).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, lay_interpreters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, orthodox_laity).
narrative_ontology:constraint_vindicates(nicene_creed_authority__strict_orthodox_reading, nicene_orthodoxy_doctrine).
narrative_ontology:constraint_vindicates(nicene_creed_authority__strict_orthodox_reading, apostolic_succession_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% They define, teach, and enforce the creed as the binding metaphysical ontology; their authority derives from apostolic succession and they benefit from the structural power to define orthodoxy and sanction deviation.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy, agenda_setter,
    institutional, generational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy, beneficiary).

% Communities that hold differing metaphysical interpretations; they bear sanctions (excommunication, anathema, legal penalties) for deviation and lose standing within the universal church.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, heterodox_communities, payer,
    organized, generational, constrained, universal).

% Individual believers who question or interpret the creed differently; they face spiritual and social sanctions, and their exit is blocked by identity fusion with the community of faith.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, lay_interpreters, payer,
    moderate, biographical, identity_locked, universal).

% Institutional bodies that adjudicate heresy cases and impose sanctions; they operationalize the clergy's authority.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, ecclesiastical_courts, agenda_setter,
    institutional, generational, analytical, universal).

% Believers who assent to the creed and gain communal identity, sacramental access, and spiritual assurance; they benefit from the unity the constraint provides.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, orthodox_laity, beneficiary,
    moderate, biographical, mobile, universal).

% Historical states that enforced heresy laws; they observed and sometimes co-opted the church's sanctioning power for political order.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, secular_authorities, observer,
    powerful, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies all believers under a single metaphysical ontology (the Nicene faith) to guarantee doctrinal unity, sacramental validity, and communal identity across the universal church.
% TRANSFER_FUNCTION: Moves interpretive authority and the power to define orthodoxy from the believing community (including lay interpreters and heterodox groups) to the hierarchical clergy and their ecclesiastical courts; the cost of deviation is borne by those who dissent.
% ABSENT_VOICES: Early dissenting bishops (e.g., Arians, Nestorians, Monophysites) who were excluded from the defining councils; modern critical historians and theologians who question the creed's metaphysical claims; laity in periods when literacy and access to councils were denied.
% DISAPPEARANCE_RATIONALE: If the creed's binding authority and heresy sanctions vanished overnight, the universal church would fracture into multiple metaphysical communities, the clergy's definitional monopoly would collapse, and the sacramental economy predicated on doctrinal unity would reorganize around local or personal faith commitments.
% FOUNDING_PROBLEM: The 4th-century church faced fragmentation over the nature of Christ and the Trinity; the creed was formulated to settle the metaphysical ontology of the faith and prevent schism by establishing a single authoritative confession.
% FOUNDING_PROBLEM_CORROBORATION: Patristic historians attest the creed solved the Arian crisis; modern scholars (e.g., R.P.C. Hanson, Lewis Ayres) argue the creed's metaphysical precision created new boundaries and exclusionary mechanisms that persist beyond the founding controversy; the hierarchical clergy maintain the problem is live (unity of faith), while heterodox traditions and critical scholars attest it is dead or transformed.
narrative_ontology:disappearance_verdict(nicene_creed_authority__strict_orthodox_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_creed_authority__strict_orthodox_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__strict_orthodox_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nicene_creed_authority__strict_orthodox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_creed_authority__strict_orthodox_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__strict_orthodox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nicene_creed_authority__strict_orthodox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nicene_creed_authority__strict_orthodox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because the creed's enforcement transfers interpretive authority and material benefits (ecclesiastical office, control of sacramental economy) to the clergy while imposing costs on dissenters. Suppression is high (0.75) because the constraint's persistence historically depended on active heresy policing (inquisitions, conciliar anathemas, state enforcement). Theater ratio is moderate (0.30): the coordination function (doctrinal unity) is real but a growing share of enforcement activity serves to protect clerical authority rather than genuine unity. Accessibility collapse (0.70) reflects that once the creed is accepted as metaphysically binding, alternative interpretations are structurally excluded. Resistance (0.50) captures persistent heterodox movements and modern critical scholarship.
 *
 * PERSPECTIVAL GAP:
 *   From the hierarchical clergy's seat, the constraint is genuine coordination (rope-like) — they experience it as preserving the faith once delivered. From heterodox communities and lay interpreters, it is extraction (snare-like) — they experience it as enforced conformity with identity-locked exit. The orthodox laity experience a mix: genuine coordination benefit with diffuse costs. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Hierarchical clergy are structural beneficiaries (d near 0.0): they collect authority, define the constraint, and face analytical exit. Heterodox communities and lay interpreters are targets (d near 1.0): they pay the transfer, face constrained or identity-locked exit. Orthodox laity sit near symmetric (d ~0.5): they receive coordination benefit but bear diffuse costs. Ecclesiastical courts are agenda_setters with analytical exit. Secular authorities are observers with analytical exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (4th-century Christological/Trinitarian controversy) is contested: the clergy claim it remains live (unity of faith requires the same metaphysical precision), while historical scholarship shows the creed's boundary function has outlived its original controversy and now serves to maintain clerical interpretive monopoly. The constraint persists not because the founding problem is unsolved but because the enforcement machinery has become self-justifying — a tangled_rope where coordination and extraction are structurally fused.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metaphysical_binding_vs_historical_contingency,
    'Does the creed''s metaphysical content genuinely coordinate believers toward a truth that would otherwise be inaccessible, or does it function as a constructed boundary that extracts interpretive authority for the clergy?',
    'Comparative analysis of communities that maintain unity without fixed metaphysical formulas (e.g., some Protestant traditions, Eastern Orthodox phronema) versus those that enforce creedal assent; historical counterfactual: if the Arian controversy had been resolved differently, would Christian unity have required a different metaphysical formula?',
    'If the metaphysical content is genuinely coordinative (truth-tracking), the extraction is the price of coordination; if it is a constructed boundary, the extraction is rent. This determines whether the constraint is tangled_rope (genuine coordination + extraction) or snare (coordination as cover).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(metaphysical_binding_vs_historical_contingency, conceptual, 'Whether the creed''s metaphysical claims are truth-apt coordinators or constructed authority markers.').

omega_variable(
    internalized_vs_structural_suppression_for_heterodox,
    'For heterodox communities and lay interpreters, is the measured suppression primarily structural (external sanctions, legal penalties) or internalized (identity fusion, spiritual terror, epistemic closure)?',
    'Post-exit trajectory studies: if former heterodox believers or dissenting theologians continue to experience spiritual anxiety, identity fragmentation, or epistemic paralysis after leaving the constraint''s jurisdiction, the suppression has an internalized component that persists beyond structural removal.',
    'If suppression is substantially internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression with them after exit. This would amplify effective extraction for identity-locked agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression_for_heterodox, empirical, 'Structural vs. internalized suppression mechanism for identity-locked victims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__strict_orthodox_reading, 0, 1700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nicene_strict_tr_t0, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(nicene_strict_tr_t0, observed).
narrative_ontology:measurement(nicene_strict_tr_t400, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 400, 0.25).
narrative_ontology:measurement_basis(nicene_strict_tr_t400, observed).
narrative_ontology:measurement(nicene_strict_tr_t800, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 800, 0.4).
narrative_ontology:measurement_basis(nicene_strict_tr_t800, observed).
narrative_ontology:measurement(nicene_strict_tr_t1200, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1200, 0.35).
narrative_ontology:measurement_basis(nicene_strict_tr_t1200, observed).
narrative_ontology:measurement(nicene_strict_tr_t1600, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1600, 0.3).
narrative_ontology:measurement_basis(nicene_strict_tr_t1600, observed).
narrative_ontology:measurement(nicene_strict_tr_t1700, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1700, 0.3).
narrative_ontology:measurement_basis(nicene_strict_tr_t1700, observed).

% Extraction over time
narrative_ontology:measurement(nicene_strict_be_t0, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(nicene_strict_be_t0, observed).
narrative_ontology:measurement(nicene_strict_be_t400, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 400, 0.55).
narrative_ontology:measurement_basis(nicene_strict_be_t400, observed).
narrative_ontology:measurement(nicene_strict_be_t800, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 800, 0.65).
narrative_ontology:measurement_basis(nicene_strict_be_t800, observed).
narrative_ontology:measurement(nicene_strict_be_t1200, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1200, 0.7).
narrative_ontology:measurement_basis(nicene_strict_be_t1200, observed).
narrative_ontology:measurement(nicene_strict_be_t1600, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1600, 0.68).
narrative_ontology:measurement_basis(nicene_strict_be_t1600, observed).
narrative_ontology:measurement(nicene_strict_be_t1700, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1700, 0.65).
narrative_ontology:measurement_basis(nicene_strict_be_t1700, observed).

% Suppression requirement over time
narrative_ontology:measurement(nicene_strict_su_t0, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(nicene_strict_su_t0, observed).
narrative_ontology:measurement(nicene_strict_su_t400, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 400, 0.6).
narrative_ontology:measurement_basis(nicene_strict_su_t400, observed).
narrative_ontology:measurement(nicene_strict_su_t800, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 800, 0.85).
narrative_ontology:measurement_basis(nicene_strict_su_t800, observed).
narrative_ontology:measurement(nicene_strict_su_t1200, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1200, 0.8).
narrative_ontology:measurement_basis(nicene_strict_su_t1200, observed).
narrative_ontology:measurement(nicene_strict_su_t1600, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1600, 0.4).
narrative_ontology:measurement_basis(nicene_strict_su_t1600, observed).
narrative_ontology:measurement(nicene_strict_su_t1700, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1700, 0.2).
narrative_ontology:measurement_basis(nicene_strict_su_t1700, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__strict_orthodox_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(nicene_creed_authority__strict_orthodox_reading, 0.08).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority__symbolic_confessional_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority__liturgical_habituation_reading).

% DUAL FORMULATION NOTE:
% This constraint is the strict_orthodox_reading of the nicene_creed_authority kernel. The symbolic_confessional_reading and liturgical_habituation_reading are sibling constraints with different ε and beneficiary/victim structures. The strict reading claims the creed's metaphysical ontology is binding and deviation is heresy; the symbolic reading treats it as contingent witness; the liturgical reading treats it as identity performance. All three share the same kernel (the Nicene Creed as a stabilized commitment) but instantiate different constraints with different structural profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nicene_creed_authority__strict_orthodox_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
