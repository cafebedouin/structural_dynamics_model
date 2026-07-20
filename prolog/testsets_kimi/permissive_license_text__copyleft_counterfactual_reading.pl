% ============================================================================
% CONSTRAINT STORY: permissive_license_text__copyleft_counterfactual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__copyleft_counterfactual_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: permissive_license_text__copyleft_counterfactual_reading
 *   human_readable: Permissive License Text â Copyleft Counterfactual Reading
 *   domain: software_licensing/intellectual_property/technology_governance
 *
 * SUMMARY:
 *   The permissive license text (MIT, BSD, Apache) relaxes copyright
 *   enforcement but omits a reciprocity requirement. In the copyleft
 *   counterfactual reading, this omission is not neutral but structurally
 *   exploitative: it coordinates genuine code reuse while enabling the
 *   enclosure of commons software by actors who contribute nothing back. The
 *   reading treats the constraint as a tangled rope where copyleft advocates
 *   benefit from the structural demand for reciprocity that permissive
 *   failure generates, and proprietary builders are victimized by the
 *   commoditization of their investments. The metrics are authored
 *   independently of the claimed type.
 *
 * KEY AGENTS:
 *   - copyleft_advocates: Primary beneficiary (organized/identity_locked) â collects legitimacy, demand, and resources from permissive licensing's failure mode
 *   - proprietary_builders: Primary target (powerful/constrained) â bears commoditization and competitive erosion costs
 *   - individual_contributors: Secondary target (moderate/constrained) â contributes labor that downstream actors capture without reciprocity
 *   - open_source_researchers: Analytical observer (analytical/analytical) â tracks licensing outcomes and enclosure dynamics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, 0.78).
domain_priors:suppression_score(permissive_license_text__copyleft_counterfactual_reading, 0.62).
domain_priors:theater_ratio(permissive_license_text__copyleft_counterfactual_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__copyleft_counterfactual_reading, tangled_rope).
narrative_ontology:human_readable(permissive_license_text__copyleft_counterfactual_reading, "Permissive License Text â Copyleft Counterfactual Reading").
narrative_ontology:topic_domain(permissive_license_text__copyleft_counterfactual_reading, "software_licensing/intellectual_property/technology_governance").

domain_priors:requires_active_enforcement(permissive_license_text__copyleft_counterfactual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__copyleft_counterfactual_reading, 'f675c7fc-267e-47c3-8757-bd663549311d').
narrative_ontology:cs_kernel_codification('f675c7fc-267e-47c3-8757-bd663549311d', fixed_text).
narrative_ontology:cs_authority_grounding('f675c7fc-267e-47c3-8757-bd663549311d', distributed).
narrative_ontology:cs_reading_relation('f675c7fc-267e-47c3-8757-bd663549311d', permissive_license_text__commons_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('f675c7fc-267e-47c3-8757-bd663549311d', permissive_license_text__corporate_moat_reading, influences).
narrative_ontology:cs_axiom('f675c7fc-267e-47c3-8757-bd663549311d', foundational, reciprocity_required_for_commons_integrity).
narrative_ontology:cs_axiom_status(reciprocity_required_for_commons_integrity, holdable).
narrative_ontology:cs_axiom_grounding('f675c7fc-267e-47c3-8757-bd663549311d', reciprocity_required_for_commons_integrity, empirically_contingent).
narrative_ontology:cs_axiom('f675c7fc-267e-47c3-8757-bd663549311d', foundational, proprietary_enclosure_without_reciprocity_is_exploitation).
narrative_ontology:cs_axiom_status(proprietary_enclosure_without_reciprocity_is_exploitation, holdable).
narrative_ontology:cs_axiom_grounding('f675c7fc-267e-47c3-8757-bd663549311d', proprietary_enclosure_without_reciprocity_is_exploitation, deontological).
narrative_ontology:cs_reference_frame('f675c7fc-267e-47c3-8757-bd663549311d', copyleft_counterfactual_origin).
narrative_ontology:cs_drift_state('f675c7fc-267e-47c3-8757-bd663549311d', contemporary_cloud_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f675c7fc-267e-47c3-8757-bd663549311d', '').
narrative_ontology:cs_kernel_id(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, copyleft_advocates).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, proprietary_builders).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, individual_contributors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promote and enforce copyleft licenses as the necessary alternative to permissive licensing. They benefit from the structural demand generated by permissive licenses' failure to require reciprocity: their licenses gain adoption, their enforcement actions gain targets, and their ideological position is validated by enclosure events. They collect donations, consulting engagements, and moral authority from communities alarmed by permissive exploitation.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, copyleft_advocates, beneficiary,
    organized, generational, identity_locked, global).

% Build proprietary products and services, often depending on permissively licensed open-source components for competitive velocity. They are victimized by the commoditization dynamic: their investments in open-source contributions can be replicated by competitors without reciprocity, and they face reputational and strategic pressure to release proprietary innovations under permissive terms to gain ecosystem adoption, eroding competitive differentiation.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, proprietary_builders, payer,
    powerful, biographical, constrained, global).

% Contribute code to permissively licensed projects under the framing of generosity and implementation freedom. Their labor is incorporated into proprietary products and cloud services without reciprocal contribution back to the commons. They lack bargaining power to change licensing terms post-hoc and typically do not capture economic value from downstream enclosure.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, individual_contributors, payer,
    moderate, biographical, constrained, global).

% Track license adoption, contribution patterns, and enclosure events across the software commons. They publish empirical analyses of how permissive terms affect sustainability and power concentration, providing analytical distance from both the advocacy and commercial seats.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, open_source_researchers, observer,
    analytical, generational, analytical, global).

narrative_ontology:fixing_cost_class(permissive_license_text__copyleft_counterfactual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Lowers legal friction for code reuse by providing a standardized, minimal-burden copyright license that enables distributed collaboration without centralized negotiation.
% TRANSFER_FUNCTION: Moves software contributions and maintenance labor from individual contributors and proprietary builders into publicly visible codebases that can be captured by downstream commercial actors without reciprocal obligation; simultaneously transfers legitimacy, consulting demand, and license adoption to copyleft advocates who position themselves as the corrective.
% ABSENT_VOICES: Small contributors who do not read or understand license implications; proprietary builders who recognize the commoditization trap but cannot speak against open source without reputational harm; users of proprietary derivatives who have no visibility into the upstream commons they depend on.
% DISAPPEARANCE_RATIONALE: If the permissive license regime vanished overnight, the software commons would fragment as reuse became legally uncertain, proprietary builders would lose the shared infrastructure they depend on for velocity, and copyleft advocates would lose the foil that validates their position â the market would reorganize around proprietary silos or copyleft mandates.
% FOUNDING_PROBLEM: Software copyright created exclusivity that prevented code reuse and collaborative improvement; early software sharing needed legal safe harbors to enable distributed development.
% FOUNDING_PROBLEM_CORROBORATION: Early software practitioners and internet historians attest to the need for sharing mechanisms. Copyleft advocates attest that the founding problem was better solved by copyleft licenses, and that permissive licenses introduced a new enclosure problem. Independent academic software historians corroborate the shift from proprietary silos to open collaboration but dispute whether permissive or copyleft licensing was the proper resolution.
narrative_ontology:disappearance_verdict(permissive_license_text__copyleft_counterfactual_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__copyleft_counterfactual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__copyleft_counterfactual_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(permissive_license_text__copyleft_counterfactual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__copyleft_counterfactual_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__copyleft_counterfactual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(permissive_license_text__copyleft_counterfactual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint systematically permits the capture of commons labor without reciprocal obligation, creating a structural transfer. Suppression is moderate-high (0.62) because the permissive regime suppresses copyleft alternatives through network effects, contributor license agreement lock-in, and social norm enforcement against 'viral' licensing. Theater ratio is moderate (0.38): much discourse around permissive licensing frames it as 'maximizing freedom,' which is performative relative to the extraction it enables. Accessibility collapse is substantial (0.65) because once a project is permissively licensed, re-licensing to copyleft requires unanimous consent. Resistance is moderate (0.58) because proprietary builders increasingly resist the commoditization dynamic through proprietary dual-licensing and source-available alternatives. Measurements share a single time grid to prevent misaligned drift dating.
 *
 * PERSPECTIVAL GAP:
 *   From the copyleft advocate seat, the permissive text is a failed coordination mechanism that demands corrective licensing; from the proprietary builder seat, it is a trap that forces commoditization; from the individual contributor seat, it appears as generosity that is captured by others. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyleft advocates are declared beneficiaries because the permissive text's failure mode generates demand for their licenses and services; their directionality is near the beneficiary pole. Proprietary builders and individual contributors are declared victims because they bear the costs of enclosure and commoditization; their directionality is near the target pole. No override is needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading prevents mislabeling by distinguishing the genuine coordination function (lowering legal friction for reuse) from the extraction function (enabling enclosure). A pure snare reading would ignore the coordination; a pure rope reading would ignore the enclosure. The tangled rope classification captures both, and the temporal measurements show extraction accumulating as the ecosystem matured and cloud commoditization intensified.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Is this constraint a reading of the permissive license text as a failed commons protector, or as a separate claim about necessary GPL alternatives?',
    'Compare structural epsilon against sibling readings; if epsilon clusters with corporate_moat_reading, the reading is extraction-focused; if it clusters with commons_coordination_reading, the coordination function dominates.',
    'Determines whether the constraint belongs to the permissive-license family as a critical variant or represents an independent copyleft advocacy claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Whether the copyleft counterfactual reading is structurally kin to the permissive text or an external critique.').

omega_variable(
    copyleft_benefit_mechanism,
    'Do copyleft advocates structurally benefit from the demand generated by permissive licensing''s failure mode, or are they net losers from commons erosion?',
    'Track license adoption trends, enforcement funding, and consulting revenue for copyleft organizations against permissive license proliferation rates.',
    'If net losers, directionality for copyleft advocates shifts toward victimhood and the beneficiary structure collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(copyleft_benefit_mechanism, empirical, 'Whether copyleft advocacy is subsidized or undermined by permissive licensing.').

omega_variable(
    proprietary_builder_victimhood,
    'Are proprietary builders genuine victims of permissive licensing commoditization, or willing participants who capture more value than they lose?',
    'Economic analysis of proprietary builder returns on permissive ecosystem participation versus closed-source alternative histories.',
    'If proprietary builders are net beneficiaries, the victim declaration is false and the classification shifts toward a different victim set.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proprietary_builder_victimhood, empirical, 'Whether proprietary builders are net payers or net capturers in the permissive regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__copyleft_counterfactual_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t0, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(perm_tr_t8, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(perm_tr_t16, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(perm_tr_t24, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement(perm_tr_t32, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 32, 0.35).
narrative_ontology:measurement(perm_tr_t40, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 40, 0.38).

% Extraction over time
narrative_ontology:measurement(perm_be_t0, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(perm_be_t8, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(perm_be_t16, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(perm_be_t24, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(perm_be_t32, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 32, 0.72).
narrative_ontology:measurement(perm_be_t40, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(perm_su_t0, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(perm_su_t8, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(perm_su_t16, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 16, 0.46).
narrative_ontology:measurement(perm_su_t24, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 24, 0.52).
narrative_ontology:measurement(perm_su_t32, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 32, 0.58).
narrative_ontology:measurement(perm_su_t40, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__copyleft_counterfactual_reading, resource_allocation).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, commons_coordination_reading).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, corporate_moat_reading).

% DUAL FORMULATION NOTE:
% One reading of the permissive_license_text kernel; decomposed because the same natural-language label covers structurally distinct claims about coordination, extraction, and counterfactual reciprocity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
