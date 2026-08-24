% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__physical_appropriation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__physical_appropriation_reading, []).

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
 *   constraint_id: takings_clause_boundary__physical_appropriation_reading
 *   human_readable: Physical Appropriation Only Takings Reading
 *   domain: constitutional/property/regulatory
 *
 * SUMMARY:
 *   The physical-appropriation reading of the Takings Clause holds that the
 *   Fifth Amendment's 'nor shall private property be taken for public use,
 *   without just compensation' applies only to direct physical seizures or
 *   permanent physical occupations of property. All regulatory diminution of
 *   value — no matter how severe — falls outside the compensation
 *   requirement. This reading emerged from early Supreme Court dicta
 *   (Pumpelly, Mugler) and was crystallized in Penn Central (1978) and
 *   subsequent precedent (Loretto, Lucas, Tahoe-Sierra). It is the operative
 *   constitutional rule governing the regulatory state's relationship to
 *   private property.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__physical_appropriation_reading, 0.85).
domain_priors:suppression_score(takings_clause_boundary__physical_appropriation_reading, 0.75).
domain_priors:theater_ratio(takings_clause_boundary__physical_appropriation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__physical_appropriation_reading, mountain).
narrative_ontology:human_readable(takings_clause_boundary__physical_appropriation_reading, "Physical Appropriation Only Takings Reading").
narrative_ontology:topic_domain(takings_clause_boundary__physical_appropriation_reading, "constitutional/property/regulatory").

domain_priors:requires_active_enforcement(takings_clause_boundary__physical_appropriation_reading).
domain_priors:emerges_naturally(takings_clause_boundary__physical_appropriation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__physical_appropriation_reading, '1370a04c-5e32-4de8-bfcb-696b6dc67bb1').
narrative_ontology:cs_kernel_codification('1370a04c-5e32-4de8-bfcb-696b6dc67bb1', formalized).
narrative_ontology:cs_authority_grounding('1370a04c-5e32-4de8-bfcb-696b6dc67bb1', lineage).
narrative_ontology:cs_interpretation_layer_present('1370a04c-5e32-4de8-bfcb-696b6dc67bb1').
narrative_ontology:cs_reading_relation('1370a04c-5e32-4de8-bfcb-696b6dc67bb1', takings_clause_boundary__categorical_takings_reading, coexists_with).
narrative_ontology:cs_reading_relation('1370a04c-5e32-4de8-bfcb-696b6dc67bb1', takings_clause_boundary__regulatory_takings_reading, forecloses).
narrative_ontology:cs_axiom('1370a04c-5e32-4de8-bfcb-696b6dc67bb1', foundational, only_physical_appropriation_triggers_compensation).
narrative_ontology:cs_axiom_status(only_physical_appropriation_triggers_compensation, holdable).
narrative_ontology:cs_axiom_grounding('1370a04c-5e32-4de8-bfcb-696b6dc67bb1', only_physical_appropriation_triggers_compensation, conventional).
narrative_ontology:cs_axiom('1370a04c-5e32-4de8-bfcb-696b6dc67bb1', secondary, regulatory_losses_are_background_risk).
narrative_ontology:cs_axiom_status(regulatory_losses_are_background_risk, holdable).
narrative_ontology:cs_axiom_grounding('1370a04c-5e32-4de8-bfcb-696b6dc67bb1', regulatory_losses_are_background_risk, conventional).
narrative_ontology:cs_reference_frame('1370a04c-5e32-4de8-bfcb-696b6dc67bb1', original_understanding_physical_takings).
narrative_ontology:cs_drift_state('1370a04c-5e32-4de8-bfcb-696b6dc67bb1', contemporary_regulatory_state, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1370a04c-5e32-4de8-bfcb-696b6dc67bb1', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__physical_appropriation_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, government_regulators).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, legislative_bodies).
narrative_ontology:constraint_victim(takings_clause_boundary__physical_appropriation_reading, property_owners_subject_to_regulation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(takings_clause_boundary__physical_appropriation_reading, regulated_industries).
narrative_ontology:constraint_vindicates(takings_clause_boundary__physical_appropriation_reading, constitutional_textualism).
narrative_ontology:constraint_vindicates(takings_clause_boundary__physical_appropriation_reading, originalist_interpretation).
narrative_ontology:constraint_vindicates(takings_clause_boundary__physical_appropriation_reading, limited_government_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce regulations across health, safety, environment, land use, and economic sectors without triggering compensation obligations for value diminution. The physical-appropriation reading insulates the regulatory state from takings liability for all non-physical regulatory impacts.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, government_regulators, agenda_setter,
    institutional, generational, arbitrage, national).

% Enact broad regulatory frameworks (zoning, environmental, labor, financial) capturing economic value from private property without budgetary compensation costs. The reading eliminates fiscal constraints on regulatory ambition.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, legislative_bodies, beneficiary,
    powerful, generational, arbitrage, national).

% Bear the full economic loss when regulations destroy property value — downzoning, wetlands designations, historic preservation, endangered species restrictions — with no constitutional recourse unless government physically occupies or seizes the land. Exit options limited to selling at depressed value or litigating under Penn Central (which this reading narrows further).
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, property_owners_subject_to_regulation, payer,
    organized, biographical, constrained, national).

% Adjudicate takings claims by applying the physical-appropriation threshold. The judiciary both observes the constraint's operation and actively shapes it through precedent — Loretto, Lucas, and Penn Central form the interpretive layer that absorbs pressure without revising the kernel.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, courts, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__physical_appropriation_reading, courts, agenda_setter).

% Absorb compliance costs and value destruction from sector-specific regulation (energy, telecom, pharma, finance). Unlike dispersed property owners, they have lobbying capacity but remain payers under this reading — regulatory costs are business expenses, not compensable takings.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, regulated_industries, payer,
    powerful, biographical, constrained, national).

% Seek to expand takings protection to regulatory overreach but find the physical-appropriation reading a doctrinal ceiling. Their arguments for compensating regulatory value loss are structurally excluded by the reading's core premise; they must either accept the frame or argue for kernel revision.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, public_interest_advocates, excluded,
    organized, biographical, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bright-line rule distinguishing per se compensable takings (physical seizure/occupation) from non-compensable regulation, giving government a stable operational domain for the regulatory state without constant compensation litigation.
% TRANSFER_FUNCTION: Moves the economic burden of regulation from the public fisc to individual property owners: when regulation destroys value, the owner bears the loss; the government regulates freely without budgetary internalization of those costs.
% ABSENT_VOICES: Property owners suffering severe regulatory value destruction (90%+ diminution) who would argue their loss is functionally equivalent to physical taking; future generations who inherit a regulatory state unconstrained by compensation discipline; indigenous communities whose land-based cultural value is destroyed by regulation without physical seizure.
% DISAPPEARANCE_RATIONALE: If the physical-appropriation-only reading vanished, regulatory takings doctrine would become the governing standard — government would face compensation obligations for regulations that go 'too far,' legislative budgets would internalize regulatory costs, property owners would gain constitutional leverage, and the regulatory state's operational calculus would fundamentally shift.
% FOUNDING_PROBLEM: How to distinguish legitimate exercises of the police power (non-compensable regulation) from exercises of eminent domain (compensable takings) under the Fifth Amendment's Takings Clause, given that both reduce property value.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars (e.g., Epstein, Barnett) attest the founding problem was preventing physical appropriation and that regulatory takings are a judicial invention. Living constitutionalists and administrative law scholars (e.g., Sax, Michelman) attest the founding problem was preventing government from shifting public burdens onto private parties regardless of mechanism, and that the physical/regulatory distinction collapses under scrutiny. No neutral corroboration exists — the dispute is structural.
narrative_ontology:disappearance_verdict(takings_clause_boundary__physical_appropriation_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__physical_appropriation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__physical_appropriation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(takings_clause_boundary__physical_appropriation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__physical_appropriation_reading, 0.85, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__physical_appropriation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(takings_clause_boundary__physical_appropriation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, ExtMetricName, E),
    domain_priors:suppression_score(takings_clause_boundary__physical_appropriation_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(takings_clause_boundary__physical_appropriation_reading),
    narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(takings_clause_boundary__physical_appropriation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.85) because the reading authorizes the regulatory state to capture nearly all regulatory value gains while externalizing virtually all regulatory value losses onto property owners. Suppression is high (0.75) because courts actively reject regulatory takings claims that would expand compensation beyond physical appropriation — the doctrinal ceiling is enforced. Theater ratio is moderate (0.4): the physical-seizure prevention function is genuine but narrow; the vast majority of the constraint's operational surface is regulatory extraction. Accessibility collapse is high (0.8) because once a regulation is characterized as 'non-physical,' the takings claim collapses almost entirely — Penn Central's ad hoc factors rarely yield compensation. Resistance is moderate (0.5): property owners litigate but face a doctrinal structure designed to suppress their claims.
 *
 * PERSPECTIVAL GAP:
 *   From the government seat, this reading is genuine coordination: a stable rule enabling the regulatory state. From the property owner seat, it is pure extraction: a constitutional rule that immunizes government from the costs of its own regulations. The engine computes this divergence from the structural data — the claimed mountain type (natural constitutional law) diverges from the computed snare/tangled_rope profile.
 *
 * DIRECTIONALITY LOGIC:
 *   Government regulators and legislative bodies are structural beneficiaries (d ≈ 0.1): they collect regulatory authority without compensation cost. Property owners and regulated industries are targets (d ≈ 0.9): they bear regulatory costs with constitutional exit blocked. Courts sit near analytical (d ≈ 0.5) but their precedent-setting role gives them agenda-setter influence. Public interest advocates are excluded (trapped exit) — they cannot access the constraint's benefit structure and their opposition is structurally filtered.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (distinguishing police power from eminent domain) has not disappeared — regulation still destroys value. But the physical-appropriation reading resolved it by defining the problem away: only physical acts count. This is mandatrophy — the constraint's mandate (preventing uncompensated takings) has atrophied into a narrow physicality test that no longer serves the original function. The reading persists because it benefits the regulatory state (agenda_setter/beneficiary) and the cost to fix (constitutional amendment or doctrinal revolution) is prohibitive for payers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physical_regulatory_boundary_coherence,
    'Is the distinction between physical appropriation and regulatory diminution a coherent constitutional principle or a formalistic line that collapses under scrutiny (e.g., permanent flooding = physical occupation per Arkansas Game & Fish; regulatory destruction of all value = taking per Lucas)?',
    'Doctrinal analysis of edge cases where physical/regulatory boundary blurs: temporary flooding, regulatory elimination of all economically viable use, physical invasions characterized as regulation. Track whether courts treat these as exceptions or boundary collapse.',
    'If the boundary is coherent, the reading has principled limits. If it collapses, the reading is a formalistic cover for extraction — the ''physical'' label becomes a results-oriented filter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physical_regulatory_boundary_coherence, conceptual, 'Whether the physical/regulatory distinction structurally holds or dissolves under pressure.').

omega_variable(
    narrow_victim_set_principled_or_result_oriented,
    'Does the reading''s narrow victim set (only physically dispossessed owners) reflect a genuine constitutional principle, or is it a result-oriented limitation that protects the regulatory state''s fiscal interests?',
    'Historical analysis of founding-era understandings of ''taking'' versus modern regulatory state incentives. Compare with state constitutional takings clauses that explicitly cover regulatory damage.',
    'If result-oriented, the reading is a snare disguised as textualism. If principled, the extraction is the price of a genuine coordination rule.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(narrow_victim_set_principled_or_result_oriented, preference, 'Whether the victim boundary is principled or instrumental.').

omega_variable(
    committer_frame_kernel_reading,
    'This constraint is one reading (physical_appropriation_reading) of the contested takings_clause_boundary kernel. What structural elements distinguish it from its siblings (categorical_takings_reading, regulatory_takings_reading)?',
    'Map the victim sets, beneficiary structures, and enforcement requirements across all three readings. The physical reading has the narrowest victim set and broadest government immunity.',
    'Documents the committer-frame structure for cross-reading analysis. The kernel''s classification depends on which reading is instantiated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_kernel_reading, conceptual, 'Commitment-system framing: this reading''s structural delta within the kernel family.').

omega_variable(
    suppression_mechanism_doctrinal_vs_political,
    'Is the high suppression (0.75) maintained primarily through doctrinal internalization (courts genuinely believe the physical/regulatory distinction) or political enforcement (appointment/confirmation pressure on judges who might expand takings)?',
    'Study judicial opinion language: do judges reason from principle or manage doctrine to avoid fiscal exposure? Track confirmation hearing dynamics on takings issues.',
    'If doctrinal internalization dominates, the constraint is more mountain-like (stable belief). If political enforcement dominates, it is more snare-like (coerced stability).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_doctrinal_vs_political, empirical, 'Whether suppression is cognitive or coercive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__physical_appropriation_reading, 1791, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(takings_physical_approp_tr_t1791, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 1791, 0.05).
narrative_ontology:measurement(takings_physical_approp_tr_t1870, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 1870, 0.1).
narrative_ontology:measurement(takings_physical_approp_tr_t1922, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 1922, 0.2).
narrative_ontology:measurement(takings_physical_approp_tr_t1978, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 1978, 0.3).
narrative_ontology:measurement(takings_physical_approp_tr_t1992, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 1992, 0.35).
narrative_ontology:measurement(takings_physical_approp_tr_t2005, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(takings_physical_approp_tr_t2026, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 2026, 0.4).

% Extraction over time
narrative_ontology:measurement(takings_physical_approp_be_t1791, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 1791, 0.15).
narrative_ontology:measurement(takings_physical_approp_be_t1870, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 1870, 0.2).
narrative_ontology:measurement(takings_physical_approp_be_t1922, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 1922, 0.35).
narrative_ontology:measurement(takings_physical_approp_be_t1978, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 1978, 0.6).
narrative_ontology:measurement(takings_physical_approp_be_t1992, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 1992, 0.7).
narrative_ontology:measurement(takings_physical_approp_be_t2005, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 2005, 0.78).
narrative_ontology:measurement(takings_physical_approp_be_t2026, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 2026, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(takings_physical_approp_su_t1791, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 1791, 0.1).
narrative_ontology:measurement(takings_physical_approp_su_t1870, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 1870, 0.15).
narrative_ontology:measurement(takings_physical_approp_su_t1922, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 1922, 0.4).
narrative_ontology:measurement(takings_physical_approp_su_t1978, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 1978, 0.55).
narrative_ontology:measurement(takings_physical_approp_su_t1992, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 1992, 0.65).
narrative_ontology:measurement(takings_physical_approp_su_t2005, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(takings_physical_approp_su_t2026, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 2026, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__physical_appropriation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(takings_clause_boundary__physical_appropriation_reading, 0.1).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, takings_clause_boundary__categorical_takings_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, takings_clause_boundary__regulatory_takings_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, regulatory_state_fiscal_discipline).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, property_rights_constitutionalism).

% DUAL FORMULATION NOTE:
% This reading is the narrowest of the three takings_clause_boundary kernel readings. It extracts maximum regulatory value for government by minimizing the compensable event set to physical appropriation only. The categorical reading adds total-value-elimination as per se taking; the regulatory reading makes excessive diminution compensable. The extraction gradient across the kernel family runs: physical_appropriation (highest extraction) > categorical > regulatory (lowest extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(takings_clause_boundary__physical_appropriation_reading, institutional, 0.1).
constraint_indexing:directionality_override(takings_clause_boundary__physical_appropriation_reading, powerful, 0.15).
constraint_indexing:directionality_override(takings_clause_boundary__physical_appropriation_reading, organized, 0.85).
constraint_indexing:directionality_override(takings_clause_boundary__physical_appropriation_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
