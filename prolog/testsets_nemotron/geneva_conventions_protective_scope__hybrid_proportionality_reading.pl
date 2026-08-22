% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__hybrid_proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__hybrid_proportionality_reading, []).

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
 *   constraint_id: geneva_conventions_protective_scope__hybrid_proportionality_reading
 *   human_readable: Geneva Conventions Protective Scope — Hybrid Proportionality Reading
 *   domain: international_humanitarian_law/legal_theory/armed_conflict_studies
 *
 * SUMMARY:
 *   This constraint story captures the hybrid proportionality reading of the
 *   Geneva Conventions' protective scope — the interpretive practice that
 *   scales humanitarian protections by conflict classification (IAC vs NIAC)
 *   and uses proportionality analysis as the bridge between AP I's detailed
 *   standards and AP II/CA3's minimal floor. The reading is contested: states
 *   use classification authority to modulate protections; victims face a
 *   protection lottery dependent on how their conflict is categorized. The
 *   kernel (Geneva protective scope) has three live readings; this story
 *   instantiates only the hybrid proportionality reading as a clean
 *   ε-invariant constraint. Other readings are separate constraint stories
 *   linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - state_actors_with_classification_authority: Primary agenda-setter/beneficiary (institutional/arbitrage) — controls conflict classification and proportionality thresholds
 *   - military_commanders_determining_proportionality: Beneficiary (powerful/constrained) — applies the calculus with operational discretion
 *   - civilian_populations_in_niac: Primary payer (powerless/trapped) — receives minimal protections, no exit from classification
 *   - detainees_in_unclassified_conflicts: Payer (powerless/trapped) — falls into protective gap when state denies IAC
 *   - non_state_armed_group_members: Excluded (moderate/constrained) — denied combatant privilege and AP II status
 *   - populations_in_occupation_without_iac_designation: Payer (powerless/identity_locked) — loses GC IV protections via classification denial
 *   - international_humanitarian_law_scholars: Observer (analytical/analytical) — documents structural divergence
 *   - international_criminal_court_prosecutors: Observer/beneficiary (institutional/analytical) — constrained by state cooperation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.68).
domain_priors:suppression_score(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.72).
domain_priors:theater_ratio(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__hybrid_proportionality_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__hybrid_proportionality_reading, "Geneva Conventions Protective Scope — Hybrid Proportionality Reading").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__hybrid_proportionality_reading, "international_humanitarian_law/legal_theory/armed_conflict_studies").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__hybrid_proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__hybrid_proportionality_reading, 'ab812212-4f1a-4cb4-9b55-aac37583a0bd').
narrative_ontology:cs_kernel_codification('ab812212-4f1a-4cb4-9b55-aac37583a0bd', formalized).
narrative_ontology:cs_authority_grounding('ab812212-4f1a-4cb4-9b55-aac37583a0bd', lineage).
narrative_ontology:cs_interpretation_layer_present('ab812212-4f1a-4cb4-9b55-aac37583a0bd').
narrative_ontology:cs_reading_relation('ab812212-4f1a-4cb4-9b55-aac37583a0bd', geneva_conventions_protective_scope__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('ab812212-4f1a-4cb4-9b55-aac37583a0bd', geneva_conventions_protective_scope__universal_rights_reading, coexists_with).
narrative_ontology:cs_axiom('ab812212-4f1a-4cb4-9b55-aac37583a0bd', foundational, conflict_classification_determines_protective_scope).
narrative_ontology:cs_axiom_status(conflict_classification_determines_protective_scope, holdable).
narrative_ontology:cs_axiom_grounding('ab812212-4f1a-4cb4-9b55-aac37583a0bd', conflict_classification_determines_protective_scope, conventional).
narrative_ontology:cs_axiom('ab812212-4f1a-4cb4-9b55-aac37583a0bd', foundational, proportionality_analysis_bridges_iac_niac_standards).
narrative_ontology:cs_axiom_status(proportionality_analysis_bridges_iac_niac_standards, holdable).
narrative_ontology:cs_axiom_grounding('ab812212-4f1a-4cb4-9b55-aac37583a0bd', proportionality_analysis_bridges_iac_niac_standards, conventional).
narrative_ontology:cs_axiom('ab812212-4f1a-4cb4-9b55-aac37583a0bd', secondary, common_article_3_as_minimum_floor_not_ceiling).
narrative_ontology:cs_axiom_status(common_article_3_as_minimum_floor_not_ceiling, holdable).
narrative_ontology:cs_axiom_grounding('ab812212-4f1a-4cb4-9b55-aac37583a0bd', common_article_3_as_minimum_floor_not_ceiling, conventional).
narrative_ontology:cs_reference_frame('ab812212-4f1a-4cb4-9b55-aac37583a0bd', id_1977_additional_protocols_framework).
narrative_ontology:cs_drift_state('ab812212-4f1a-4cb4-9b55-aac37583a0bd', contemporary_asymmetric_conflict_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ab812212-4f1a-4cb4-9b55-aac37583a0bd', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, state_actors_with_classification_authority).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, military_commanders_determining_proportionality).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilian_populations_in_niac).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, detainees_in_unclassified_conflicts).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, non_state_armed_group_members).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, populations_in_occupation_without_iac_designation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, international_criminal_court_prosecutors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States classify conflicts as IAC or NIAC to determine applicable legal standards. This classification power lets them selectively invoke AP I's robust protections (for IAC) or AP II/CA3's minimal floor (for NIAC). They control the proportionality calculus that determines civilian harm thresholds. Exit from this role means ceding interpretive authority — rare for sovereigns.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, state_actors_with_classification_authority, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__hybrid_proportionality_reading, state_actors_with_classification_authority, beneficiary).

% Commanders apply proportionality analysis in targeting decisions. The hybrid reading's ambiguity lets them weigh military advantage against civilian harm using conflict-type-dependent standards. In IAC they face AP I's detailed rules; in NIAC they operate under CA3's vague prohibitions plus customary law. Their institutional position depends on maintaining operational discretion.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, military_commanders_determining_proportionality, beneficiary,
    powerful, biographical, constrained, regional).

% Civilians in non-international armed conflicts receive AP II/CA3 protections only — no combatant privilege, no prisoner-of-war status, limited rules on conduct of hostilities. They cannot exit the conflict zone, cannot choose their legal classification, and bear the full weight of the weaker standard. Their victimhood is structural: the constraint's classification mechanism places them in the lower-protection tier.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilian_populations_in_niac, payer,
    powerless, biographical, trapped, local).

% Persons detained in conflicts where the state refuses IAC designation (e.g., 'counter-terrorism operations,' 'internal disturbances'). They fall into a protective gap: not POWs under GC III, not fully protected civilians under GC IV, and the state argues AP II doesn't apply. The proportionality calculus is bypassed entirely — they are outside the constraint's coordination function.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, detainees_in_unclassified_conflicts, payer,
    powerless, biographical, trapped, local).

% Fighters in organized armed groups not belonging to state armed forces. Under the hybrid reading they have no combatant privilege (no IAC status) and AP II grants them no status — only CA3's humane treatment floor. They would object to being denied both POW protections and the ability to claim prisoner status, but they have no voice in treaty interpretation.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, non_state_armed_group_members, excluded,
    moderate, biographical, constrained, regional).

% Populations under effective control of a foreign power where the occupier denies IAC status (e.g., 'administered territories,' 'disputed areas'). They lose GC IV's occupation law protections because the conflict is classified NIAC. Their identity is fused to the territory; exit means displacement. The proportionality analysis that should govern targeting becomes inapplicable — they are structurally invisible to the coordination function.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, populations_in_occupation_without_iac_designation, payer,
    powerless, generational, identity_locked, regional).

% Analyze the constraint's operation from outside the enforcement structure. They document how conflict classification drifts, how proportionality calculi diverge, and where protective gaps emerge. Their analytical seat has no stake in the constraint's persistence — they observe the structural divergence between claimed coordination and actual extraction.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, international_humanitarian_law_scholars, observer,
    analytical, civilizational, analytical, universal).

% Prosecute war crimes under the Rome Statute, which incorporates both IAC and NIAC thresholds. They benefit from the hybrid reading's detailed rules (more chargeable conduct) but are constrained by state non-cooperation. Their role is partly analytical (documenting violations) and partly beneficiary (the constraint's complexity expands their jurisdictional reach).
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, international_criminal_court_prosecutors, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__hybrid_proportionality_reading, international_criminal_court_prosecutors, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the application of humanitarian protections across different conflict types by providing a classification framework (IAC vs NIAC) and a proportionality calculus that scales protections to the conflict's legal character — solving the problem of how to regulate diverse armed conflicts under a single treaty regime.
% TRANSFER_FUNCTION: Transfers protective coverage from weaker parties (civilians, detainees, non-state fighters) to stronger parties (states with classification authority, commanders with targeting discretion) through the mechanism of conflict classification and proportionality analysis. The arrangement moves legal protection downward in NIAC and upward in IAC, with the classification decision controlled by the stronger party.
% ABSENT_VOICES: Affected populations in contested classification zones (e.g., Gaza, Kashmir, Donbas pre-2022, Sahel conflict zones) — they would object to being placed in the NIAC tier when IAC-level hostilities occur, but they have no standing in treaty interpretation bodies. Non-state armed groups seeking recognition as parties to conflict are structurally excluded from the classification decision.
% DISAPPEARANCE_RATIONALE: If the hybrid proportionality reading vanished, states would lose the legal framework that lets them scale protections by conflict classification. The immediate effect would be either (a) universal application of AP I standards (if universal_rights_reading filled the gap) or (b) reversion to state-centric minimalism (if state_centric_reading prevailed). Either way, the protective landscape reorganizes — classification authority disappears, proportionality calculus loses its conflict-type anchor, and victims' legal status becomes contested in a new way.
% FOUNDING_PROBLEM: The 1949 Geneva Conventions created a binary IAC/NIAC framework (Common Article 3) that left NIAC protections radically underdeveloped. The 1977 Additional Protocols attempted to fix this: AP I elaborated IAC rules, AP II created a minimal NIAC treaty. The hybrid proportionality reading emerged as the interpretive practice that bridges the two — using proportionality analysis to determine which standards apply when conflict classification is uncertain or contested.
% FOUNDING_PROBLEM_CORROBORATION: ICRC commentaries attest the founding problem (NIAC protection gap) remains live — contemporary conflicts increasingly blur IAC/NIAC lines. State practice (e.g., US 'war on terror' classification, Russian 'special military operation' framing) corroborates that the classification mechanism is actively used to modulate protections. Scholars outside the benefiting parties (Sassòli, Dörmann, Lubell) document the drift from AP II's minimal floor toward functional IAC standards in high-intensity NIACs — but states resist codifying this drift.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__hybrid_proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__hybrid_proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__hybrid_proportionality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(geneva_conventions_protective_scope__hybrid_proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) reflects the systematic transfer of protective coverage from weaker parties to state classification authority — the constraint extracts protections from NIAC civilians/detainees to fund the coordination function of a unified treaty regime. Suppression (0.72) is high because the classification mechanism actively excludes alternative protective frameworks (human rights law applicability, universal floor arguments) and denies standing to affected populations. Theater ratio (0.41) is substantial: the proportionality calculus performs a coordination function (real) but increasingly serves as cover for classification-driven protection modulation. Accessibility collapse (0.38) is moderate — alternatives exist (universal rights reading, state-centric reading) but are structurally suppressed. Resistance (0.55) is significant: ICRC, courts, and civil society push back against classification abuse, but the constraint's institutional architecture absorbs challenges.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter/beneficiary seats (states, commanders) experience this as genuine coordination — a workable framework for regulating diverse conflicts. The payer seats (NIAC civilians, detainees, occupied populations) experience it as extraction — their protections are the currency that buys the regime's coherence. The excluded seat (non-state fighters) experiences it as foreclosure — no path to status. The observer seats see the structural divergence: the constraint's coordination function is real but its extraction is built into the classification mechanism itself. The engine computes this divergence from power/exit/role declarations; the claim (tangled_rope) reflects the hybrid nature — coordination AND extraction through the same structure.
 *
 * DIRECTIONALITY LOGIC:
 *   States with classification authority are structural beneficiaries (d ≈ 0.15) — they collect interpretive control and protective flexibility. Commanders are beneficiaries (d ≈ 0.25) — they gain operational discretion scaled by conflict type. NIAC civilians are full targets (d ≈ 0.95) — trapped, powerless, bearing the protection deficit. Detainees in unclassified conflicts are full targets (d ≈ 0.98) — the constraint's coordination function explicitly bypasses them. Non-state fighters are excluded (d ≈ 0.85) — their exclusion IS the enforcement mechanism. Occupied populations without IAC designation are identity-locked targets (d ≈ 0.9) — territorial identity fuses them to the classification denial. Scholars and prosecutors are analytical (d ≈ 0.5) — symmetric observers with institutional stakes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (NIAC protection gap) remains contested — not dead, not fully live. The hybrid reading prevents mislabeling: it is not pure coordination (rope) because classification authority extracts protections from the weak; not pure extraction (snare) because AP I/AP II/CA3 do solve real coordination problems (common standards, demarcation lines, minimum floor); not a scaffold because no sunset — the binary IAC/NIAC architecture is treated as permanent. The mandatrophy risk is that the classification mechanism becomes a permanent extraction engine: states retain the coordination benefits while the NIAC floor atrophies. The reading_relations and axioms in cs_structure capture how this reading positions itself against siblings to avoid capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    classification_authority_capture,
    'Does the conflict classification mechanism serve a genuine coordination function (sorting conflicts for appropriate regulation) or has it been captured as a tool for protection modulation by the stronger party?',
    'Compare state classification practice against ICRC typology and judicial findings (ICTY Tadić, ICJ Nicaragua, ICC situation decisions). If classifications consistently track factual intensity/organization, coordination function dominates; if they track political convenience, capture dominates.',
    'If captured, the constraint''s extraction is structural — the coordination story is cover. If genuine, extraction is the price of a workable classification system. Affects whether tangled_rope is the right claim or whether snare better describes the NIAC tier.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classification_authority_capture, empirical, 'Whether conflict classification is a coordination tool or an extraction lever.').

omega_variable(
    proportionality_calculus_separability,
    'Is the proportionality analysis structurally separable from conflict classification, or does the calculus itself embed the classification bias (e.g., ''military advantage'' defined differently in IAC vs NIAC)?',
    'Analyze targeting doctrine manuals (US DoD Law of War Manual, UK Joint Doctrine, NATO AJP-3.9) and judicial reasoning (ICC Al Hassan, ICTY Gotovina) for whether proportionality methodology changes with conflict classification or applies uniformly.',
    'If inseparable, the proportionality calculus is part of the extraction mechanism — the coordination function is the calculus itself, biased by classification. If separable, the calculus could be a genuine coordination bridge independent of classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proportionality_calculus_separability, conceptual, 'Whether proportionality analysis is an independent coordination function or classification-dependent.').

omega_variable(
    niac_customary_convergence,
    'Has customary international law converged IAC and NIAC proportionality standards such that the hybrid reading''s extraction is decreasing over time?',
    'Track state practice and opinio juris in high-intensity NIACs (Syria, Yemen, Ukraine pre-2022, Tigray) for convergence on AP I-type proportionality rules. Monitor ICRC customary law study updates and UN-mandated commission findings.',
    'If converging, base_extractiveness should trend downward (less protection transfer) and the constraint may drift toward rope. If diverging (states resisting convergence), extraction persists or grows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(niac_customary_convergence, empirical, 'Whether customary law is closing the IAC/NIAC protection gap.').

omega_variable(
    commitment_system_framing_ambiguity,
    'Is the Geneva Conventions regime best framed as a lineage-based commitment system (authority grounded in treaty text continuity) or an extraction-based commitment system (authority grounded in states'' benefit from preventing revision)?',
    'Assess whether the treaty''s amendment mechanisms (Art. 158 GC IV, Art. 99 AP I) have ever been successfully used, or whether all significant development occurs through interpretation (ICRC commentaries, judicial decisions, soft law). If interpretation absorbs all drift, the system functions as extraction-grounded.',
    'If extraction-grounded, the cs_structure authority_grounding should be ''extraction'' not ''lineage'' — the constraint''s persistence depends on states blocking formal revision while controlling interpretation. This affects the drift_state analysis and axiom status assignments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commitment_system_framing_ambiguity, conceptual, 'Whether the commitment system''s authority derives from textual continuity or revision prevention.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__hybrid_proportionality_reading, 1977, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(geneva_prot_scope_hybrid_prop_tr_t1977, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 1977, 0.15).
narrative_ontology:measurement(geneva_prot_scope_hybrid_prop_tr_t1995, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 1995, 0.22).
narrative_ontology:measurement(geneva_prot_scope_hybrid_prop_tr_t2001, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 2001, 0.33).
narrative_ontology:measurement(geneva_prot_scope_hybrid_prop_tr_t2011, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 2011, 0.37).
narrative_ontology:measurement(geneva_prot_scope_hybrid_prop_tr_t2014, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 2014, 0.39).
narrative_ontology:measurement(geneva_prot_scope_hybrid_prop_tr_t2022, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 2022, 0.41).

% Extraction over time
narrative_ontology:measurement(geneva_prot_scope_hybrid_prop_be_t1977, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 1977, 0.35).
narrative_ontology:measurement(geneva_prot_scope_hybrid_prop_be_t1995, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 1995, 0.42).
narrative_ontology:measurement(geneva_prot_scope_hybrid_prop_be_t2001, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 2001, 0.58).
narrative_ontology:measurement(geneva_prot_scope_hybrid_prop_be_t2011, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 2011, 0.61).
narrative_ontology:measurement(geneva_prot_scope_hybrid_prop_be_t2014, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 2014, 0.65).
narrative_ontology:measurement(geneva_prot_scope_hybrid_prop_be_t2022, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 2022, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(geneva_prot_scope_hybrid_prop_su_t1977, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 1977, 0.45).
narrative_ontology:measurement(geneva_prot_scope_hybrid_prop_su_t1995, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 1995, 0.52).
narrative_ontology:measurement(geneva_prot_scope_hybrid_prop_su_t2001, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 2001, 0.68).
narrative_ontology:measurement(geneva_prot_scope_hybrid_prop_su_t2011, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 2011, 0.7).
narrative_ontology:measurement(geneva_prot_scope_hybrid_prop_su_t2014, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 2014, 0.71).
narrative_ontology:measurement(geneva_prot_scope_hybrid_prop_su_t2022, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 2022, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__hybrid_proportionality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.12).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope__state_centric_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope__universal_rights_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, additional_protocol_i_customary_status).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, common_article_3_minimum_standards).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, distinction_principle).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, proportionality_principle).

% DUAL FORMULATION NOTE:
% This constraint (hybrid_proportionality_reading) is one of three readings of the geneva_conventions_protective_scope kernel. The state_centric_reading forecloses universal protections for unprivileged belligerents; the universal_rights_reading forecloses the IAC/NIAC classification as a protection modulator. This reading coexists with both (different parties hold each) but influences both: it creates the classification/proportionality framework that the state_centric reading uses to exclude, and that the universal_rights reading must overcome. The three stories form a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_protective_scope__hybrid_proportionality_reading, institutional, 0.15).
constraint_indexing:directionality_override(geneva_conventions_protective_scope__hybrid_proportionality_reading, powerful, 0.25).
constraint_indexing:directionality_override(geneva_conventions_protective_scope__hybrid_proportionality_reading, powerless, 0.95).
constraint_indexing:directionality_override(geneva_conventions_protective_scope__hybrid_proportionality_reading, moderate, 0.85).
constraint_indexing:directionality_override(geneva_conventions_protective_scope__hybrid_proportionality_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
