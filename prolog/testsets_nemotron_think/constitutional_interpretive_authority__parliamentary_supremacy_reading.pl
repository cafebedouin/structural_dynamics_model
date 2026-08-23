% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__parliamentary_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__parliamentary_supremacy_reading, []).

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
 *   constraint_id: constitutional_interpretive_authority__parliamentary_supremacy_reading
 *   human_readable: Parliamentary Supremacy in Constitutional Interpretation
 *   domain: constitutional_law/political_theory/jurisprudence
 *
 * SUMMARY:
 *   This constraint story instantiates the parliamentary_supremacy_reading of
 *   the constitutional_interpretive_authority kernel. The reading holds that
 *   the elected legislature possesses final interpretive authority over
 *   constitutional meaning, with no judicial power to void parliamentary
 *   acts. This is the classical Westminster model (UK, New Zealand,
 *   historically Canada pre-1982). The constraint coordinates constitutional
 *   interpretation by vesting final authority in the democratically elected
 *   branch, while extracting interpretive discretion for legislative
 *   majorities — creating a genuine coordination function (resolving
 *   interpretive disputes) alongside asymmetric extraction (legislature gains
 *   discretionary power that can be used to entrench majority preferences).
 *   The legitimating narrative is electoral mandate rather than
 *   rights-grounding.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.45).
domain_priors:suppression_score(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.35).
domain_priors:theater_ratio(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__parliamentary_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__parliamentary_supremacy_reading, "Parliamentary Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_interpretive_authority__parliamentary_supremacy_reading, "constitutional_law/political_theory/jurisprudence").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__parliamentary_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__parliamentary_supremacy_reading, 'dbea761b-e3f6-42d3-83f0-79ca1d69cacc').
narrative_ontology:cs_kernel_codification('dbea761b-e3f6-42d3-83f0-79ca1d69cacc', formalized).
narrative_ontology:cs_authority_grounding('dbea761b-e3f6-42d3-83f0-79ca1d69cacc', lineage).
narrative_ontology:cs_interpretation_layer_present('dbea761b-e3f6-42d3-83f0-79ca1d69cacc').
narrative_ontology:cs_reading_relation('dbea761b-e3f6-42d3-83f0-79ca1d69cacc', constitutional_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('dbea761b-e3f6-42d3-83f0-79ca1d69cacc', constitutional_interpretive_authority__coordinate_construction_reading, coexists_with).
narrative_ontology:cs_axiom('dbea761b-e3f6-42d3-83f0-79ca1d69cacc', foundational, parliamentary_sovereignty_principle).
narrative_ontology:cs_axiom_status(parliamentary_sovereignty_principle, holdable).
narrative_ontology:cs_axiom_grounding('dbea761b-e3f6-42d3-83f0-79ca1d69cacc', parliamentary_sovereignty_principle, conventional).
narrative_ontology:cs_axiom('dbea761b-e3f6-42d3-83f0-79ca1d69cacc', foundational, electoral_legitimacy_grounds_interpretive_authority).
narrative_ontology:cs_axiom_status(electoral_legitimacy_grounds_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('dbea761b-e3f6-42d3-83f0-79ca1d69cacc', electoral_legitimacy_grounds_interpretive_authority, instrumental).
narrative_ontology:cs_reference_frame('dbea761b-e3f6-42d3-83f0-79ca1d69cacc', parliamentary_sovereignty_framework).
narrative_ontology:cs_drift_state('dbea761b-e3f6-42d3-83f0-79ca1d69cacc', contemporary_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('dbea761b-e3f6-42d3-83f0-79ca1d69cacc', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__parliamentary_supremacy_reading, parliament).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__parliamentary_supremacy_reading, government).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, judiciary).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, rights_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__parliamentary_supremacy_reading, citizens).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__parliamentary_supremacy_reading, parliamentary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__parliamentary_supremacy_reading, electoral_mandate_legitimacy).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__parliamentary_supremacy_reading, democratic_legitimacy_of_interpretation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possesses final interpretive authority over constitutional meaning; enacts legislation that cannot be voided by courts; benefits from discretionary interpretive power that allows legislative majorities to shape constitutional application. Exit from this role would require constitutional revolution or fundamental reform.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, parliament, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__parliamentary_supremacy_reading, parliament, beneficiary).

% Executive branch benefits from parliamentary supremacy as it typically controls parliamentary majority; gains interpretive discretion in implementing legislation without judicial second-guessing of constitutional compatibility. Constrained by need to maintain parliamentary confidence and electoral accountability.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, government, beneficiary,
    powerful, biographical, constrained, national).

% Formally excluded from authority to void parliamentary acts; retains interpretive role through statutory interpretation and common law development but cannot strike down primary legislation. Bound by constitutional duty to apply parliamentary will; exit from this subordinate role would require constitutional amendment or revolutionary change.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, judiciary, payer,
    institutional, generational, constrained, national).

% Individuals and groups seeking rights protection lose the safeguard of judicial nullification of rights-infringing legislation; must rely on political process, parliamentary self-restraint, or declaratory remedies that parliament may ignore. Exit options limited to political mobilization or emigration.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, rights_claimants, payer,
    moderate, biographical, constrained, national).

% Benefit from democratic accountability: constitutional interpretation remains with elected representatives who face electoral consequences; can punish legislative overreach at ballot box. Exit through voting, political participation, or migration; not structurally trapped by the constraint.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, citizens, beneficiary,
    organized, biographical, mobile, national).

% Analyze and critique the parliamentary supremacy model from outside the constitutional order; no direct stake in its operation but shape the intellectual framework through which its legitimacy is assessed across jurisdictions and eras.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves constitutional interpretive disputes through a single, democratically accountable authority — the elected legislature — avoiding the coordination failure of competing interpretive claims by courts, executives, and legislatures.
% TRANSFER_FUNCTION: Moves final interpretive authority from the judiciary to the elected legislature; legitimacy flows from electoral mandate rather than rights-grounding or legal expertise; the constraint transfers the power to determine constitutional meaning from unelected judges to elected representatives.
% ABSENT_VOICES: Minority communities, rights advocates, and constitutional theorists who argue that rights protection requires a counter-majoritarian judicial check — structurally excluded because the constraint's legitimating logic (electoral mandate) treats their exclusion as democratic legitimacy rather than democratic deficit.
% DISAPPEARANCE_RATIONALE: If parliamentary supremacy vanished overnight, judicial review of primary legislation would become the default, fundamentally restructuring the constitutional order: courts would gain veto power over legislation, the legislative-executive relationship would shift, and the democratic legitimacy narrative would invert from parliamentary accountability to judicial guardianship.
% FOUNDING_PROBLEM: The need for a single, democratically legitimate authority to resolve constitutional interpretive disputes in a system where the legislature represents the popular will — avoiding the perceived democratic deficit of unelected judges overriding elected representatives on fundamental constitutional questions.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians (Dicey, Jennings, Bradley) attest the founding problem as live in the classical parliamentary tradition; rights theorists (Dworkin, Waldron, Allan) and comparative constitutional scholars contest it, arguing that democratic legitimacy requires judicial protection of minority rights against legislative majorities — corroboration from outside the beneficiary set (parliament/government) exists on both sides.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__parliamentary_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__parliamentary_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__parliamentary_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_interpretive_authority__parliamentary_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__parliamentary_supremacy_reading_tests).
:- end_tests(constitutional_interpretive_authority__parliamentary_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) reflects the legislature's discretionary interpretive power that can be exercised to favor majority interests; suppression (0.35) captures the structural exclusion of judicial nullification — not coercive in the traditional sense but a constitutional design choice that suppresses an alternative interpretive authority; theater_ratio (0.15) is low because the constraint is functionally operative — parliamentary sovereignty is real, not performative. The temporal series shows gradual increases as rights consciousness grows and judicial dialogue develops (HRA 1998, common law constitutionalism), creating pressure on the classical model without formally displacing it.
 *
 * PERSPECTIVAL GAP:
 *   From parliament's seat (agenda_setter/beneficiary), the constraint is coordination: democratic legitimacy requires final interpretive authority to rest with elected representatives. From the judiciary's seat (payer), it is extraction: their expert constitutional judgment is structurally subordinated to political majorities. From rights_claimants (payer), it is snare-like: the coordination story (democratic accountability) provides cover for the absence of enforceable rights protection. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Parliament and government are beneficiaries (d near 0.0) — they collect interpretive discretion and legitimating narrative. Judiciary and rights_claimants are payers (d near 1.0) — they bear the cost of excluded authority and absent judicial remedy. Citizens are beneficiaries (d ~ 0.3) — democratic accountability is real but diffuse. Constitutional_scholars are observers (d = 0.5). The electoral mandate legitimating story dampens effective extraction for beneficiaries but does not eliminate the structural asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (democratic legitimacy of constitutional interpretation) remains contested — not dead, not universally live. The constraint persists not because the founding problem is solved but because the coordination function (single authoritative interpreter) remains structurally necessary and the extraction (legislative discretion) is legitimated by the very democratic theory the constraint instantiates. No concentrated beneficiary captures the extraction in a way that would make this a snare; the beneficiary (parliament) is also the agenda_setter, and the extraction is the coordination mechanism itself — classic tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the constitutional_interpretive_authority kernel, or a standalone constitutional principle?',
    'Structural comparison with sibling readings: if the same constitutional text/practice generates mutually exclusive interpretive authority allocations across readings, they are kernel readings; if they describe different constitutional orders, they are distinct constraints.',
    'If kernel reading, ε is reading-indexed over a fixed referent (the constitutional arrangement); if standalone, ε describes the arrangement itself. Affects how extraction is attributed and whether classification divergence across readings is measured.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this constraint is one reading of a contested kernel or an independent constraint.').

omega_variable(
    rights_protection_effectiveness,
    'Does parliamentary supremacy actually provide better rights protection than judicial review through political accountability mechanisms?',
    'Comparative empirical study of rights outcomes in parliamentary supremacy vs. judicial supremacy systems, controlling for political culture and institutional design.',
    'If parliamentary systems protect rights equally or better, the extraction (loss of judicial remedy) is offset by coordination benefit (democratic accountability) — supports rope/tangled_rope. If systematically worse, the constraint leans snare for rights_claimants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rights_protection_effectiveness, empirical, 'Whether the coordination function delivers its promised benefit to the claimed beneficiaries (citizens/rights_claimants).').

omega_variable(
    coordinate_construction_viability,
    'Is the coordinate_construction_reading a structurally viable alternative (genuine inter-branch dialogue without final authority) or an unstable equilibrium that collapses into one of the other two readings?',
    'Historical analysis of constitutional systems attempting coordinate construction (e.g., Canada 1982-2024, UK post-HRA 1998): do they stabilize or drift toward parliamentary or judicial supremacy?',
    'If coordinate construction is unstable, the kernel has two stable attractors (parliamentary/judicial supremacy) — this reading''s relation to judicial_supremacy_reading becomes forecloses. If stable, all three coexist.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordinate_construction_viability, empirical, 'Whether the third reading represents a genuine structural alternative or a transitional state.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__parliamentary_supremacy_reading, 1688, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cia_parl_sup_tr_t1688, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 1688, 0.05).
narrative_ontology:measurement(cia_parl_sup_tr_t1832, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 1832, 0.08).
narrative_ontology:measurement(cia_parl_sup_tr_t1911, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 1911, 0.1).
narrative_ontology:measurement(cia_parl_sup_tr_t1972, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 1972, 0.12).
narrative_ontology:measurement(cia_parl_sup_tr_t1998, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 1998, 0.14).
narrative_ontology:measurement(cia_parl_sup_tr_t2024, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(cia_parl_sup_be_t1688, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 1688, 0.25).
narrative_ontology:measurement(cia_parl_sup_be_t1832, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 1832, 0.3).
narrative_ontology:measurement(cia_parl_sup_be_t1911, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 1911, 0.35).
narrative_ontology:measurement(cia_parl_sup_be_t1972, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 1972, 0.4).
narrative_ontology:measurement(cia_parl_sup_be_t1998, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 1998, 0.42).
narrative_ontology:measurement(cia_parl_sup_be_t2024, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(cia_parl_sup_su_t1688, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 1688, 0.2).
narrative_ontology:measurement(cia_parl_sup_su_t1832, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 1832, 0.25).
narrative_ontology:measurement(cia_parl_sup_su_t1911, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 1911, 0.3).
narrative_ontology:measurement(cia_parl_sup_su_t1972, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 1972, 0.32).
narrative_ontology:measurement(cia_parl_sup_su_t1998, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 1998, 0.34).
narrative_ontology:measurement(cia_parl_sup_su_t2024, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__parliamentary_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_interpretive_authority__coordinate_construction_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, human_rights_act_1998_dialogue_model).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, common_law_constitutionalism).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the constitutional_interpretive_authority kernel into three structurally distinct readings with different ε values and beneficiary/victim structures. Parliamentary supremacy (this story) has moderate extraction (0.45) with legislature as beneficiary; judicial supremacy would have lower extraction from legislature but higher from rights_claimants; coordinate construction claims minimal extraction but its coordination function is empirically contested.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_interpretive_authority__parliamentary_supremacy_reading, institutional, 0.15).
constraint_indexing:directionality_override(constitutional_interpretive_authority__parliamentary_supremacy_reading, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
