% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__restrictive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: lausanne_minority_protections__restrictive_reading
 *   human_readable: Lausanne Minority Protections — Restrictive Reading (Individual Worship Only)
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   The restrictive reading of Lausanne minority protections limits treaty
 *   guarantees to individual worship rights (Article 38-44), treating
 *   institutional autonomy, property ownership, and theological education as
 *   domestic matters subject to general Turkish law. This reading has been
 *   the operative interpretation of the Turkish state since the 1930s,
 *   progressively implemented through vakif law, property expropriation,
 *   closure of minority schools, and the 1971 closure of Halki Seminary. The
 *   constraint operates as a snare: it presents a coordination facade
 *   (individual worship guaranteed) while extracting institutional capacity
 *   from minority communities through active enforcement (court rulings,
 *   administrative decisions, legislative restrictions). The beneficiary is
 *   the Turkish state apparatus consolidating control; the victims are
 *   minority religious institutions, theological education, and property
 *   holders. This is one reading of the contested Lausanne kernel; the
 *   expansive and guarantor readings instantiate different constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__restrictive_reading, 0.82).
domain_priors:suppression_score(lausanne_minority_protections__restrictive_reading, 0.88).
domain_priors:theater_ratio(lausanne_minority_protections__restrictive_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__restrictive_reading, snare).
narrative_ontology:human_readable(lausanne_minority_protections__restrictive_reading, "Lausanne Minority Protections — Restrictive Reading (Individual Worship Only)").
narrative_ontology:topic_domain(lausanne_minority_protections__restrictive_reading, "international_law/religious_governance/minority_rights").

domain_priors:requires_active_enforcement(lausanne_minority_protections__restrictive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__restrictive_reading, 'cf5cd1e4-5137-4814-90bd-14420b421049').
narrative_ontology:cs_kernel_codification('cf5cd1e4-5137-4814-90bd-14420b421049', formalized).
narrative_ontology:cs_authority_grounding('cf5cd1e4-5137-4814-90bd-14420b421049', extraction).
narrative_ontology:cs_interpretation_layer_present('cf5cd1e4-5137-4814-90bd-14420b421049').
narrative_ontology:cs_reading_relation('cf5cd1e4-5137-4814-90bd-14420b421049', lausanne_minority_protections__expansive_reading, forecloses).
narrative_ontology:cs_reading_relation('cf5cd1e4-5137-4814-90bd-14420b421049', lausanne_minority_protections__guarantor_reading, coexists_with).
narrative_ontology:cs_axiom('cf5cd1e4-5137-4814-90bd-14420b421049', foundational, minority_protections_exhausted_in_individual_worship).
narrative_ontology:cs_axiom_status(minority_protections_exhausted_in_individual_worship, holdable).
narrative_ontology:cs_axiom_grounding('cf5cd1e4-5137-4814-90bd-14420b421049', minority_protections_exhausted_in_individual_worship, conventional).
narrative_ontology:cs_axiom('cf5cd1e4-5137-4814-90bd-14420b421049', foundational, institutional_autonomy_domestic_competence).
narrative_ontology:cs_axiom_status(institutional_autonomy_domestic_competence, holdable).
narrative_ontology:cs_axiom_grounding('cf5cd1e4-5137-4814-90bd-14420b421049', institutional_autonomy_domestic_competence, conventional).
narrative_ontology:cs_reference_frame('cf5cd1e4-5137-4814-90bd-14420b421049', restrictive_lausanne_interpretation).
narrative_ontology:cs_drift_state('cf5cd1e4-5137-4814-90bd-14420b421049', post_1923_implementation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('cf5cd1e4-5137-4814-90bd-14420b421049', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, minority_religious_institutions).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, minority_theological_education).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, minority_property_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__restrictive_reading, individual_minority_believers).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, individual_minority_believers).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__restrictive_reading, state_sovereignty_over_religious_institutions).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__restrictive_reading, domestic_competence_in_minority_affairs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls interpretation and enforcement of Lausanne Treaty minority provisions through domestic courts and administrative bodies. Consolidates authority over minority institutional assets (properties, schools, legal personality) by treating them as domestic matters. Collects institutional capacity and property value from minority communities while providing only individual worship guarantees.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus, beneficiary).

% Pre-1923 religious foundations (vakifs), patriarchates, and communal governance structures. Face property confiscation, denial of legal personality, and inability to maintain communal assets. Their institutional continuity depends on the expansive reading; under restrictive reading they are structurally foreclosed from self-administration. Exit requires abandoning communal religious identity.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, minority_religious_institutions, payer,
    powerless, generational, identity_locked, national).

% Clergy formation institutions (e.g., Halki Seminary for Ecumenical Patriarchate, Armenian and Greek Orthodox schools). Subject to general Turkish education law with no special provisions for minority theological training. Cannot operate independently; state controls curriculum, appointments, and licensing. No viable exit — closure means end of indigenous clergy formation.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, minority_theological_education, payer,
    powerless, generational, trapped, national).

% Communal and individual property holders within minority communities. Subject to general Turkish property law including expropriation, inheritance restrictions, and vakif administration by state-appointed boards. Lose property through restrictive interpretation of 'domestic matter' — no treaty protection for institutional assets. Limited exit: can sell but cannot transfer to communal structures.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, minority_property_holders, payer,
    powerless, biographical, constrained, national).

% Receive the sole guaranteed protection: individual worship rights (attend services, personal prayer). But lose institutional vehicles that sustain worship across generations — no clergy, no communal property, no schools. Indirectly bear extraction through institutional collapse. Can exit by emigration or assimilation; mobile compared to institutions.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, individual_minority_believers, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__restrictive_reading, individual_minority_believers, payer).

% Original Lausanne signatories (UK, France, Italy, Japan, Greece, etc.) with treaty guarantee role. Structurally excluded from interpretive authority under restrictive reading — Turkey asserts sole domestic competence. Would object to restrictive interpretation but have no enforcement mechanism within this reading's framework.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, guarantor_states, excluded,
    institutional, generational, analytical, global).

% Adjudicates individual applications under ECHR. Has developed jurisprudence (e.g., Hasan and Chaush v. Bulgaria, Fener Rum Patrikhanesi v. Turkey) that partially contradicts restrictive reading by recognizing institutional dimensions. But cannot bind treaty interpretation directly; operates at individual rights level only.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, european_court_human_rights, observer,
    institutional, generational, analytical, continental).

% Analyze treaty text, drafting history, and state practice. Split between expansive and restrictive readings. Provide the analytical seat that sees the full structural divergence between readings.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a minimal, non-negotiable floor: individual worship cannot be prohibited. Solves the coordination problem of preventing outright religious persecution while leaving all institutional architecture to domestic discretion.
% TRANSFER_FUNCTION: Moves institutional autonomy, property control, and clergy formation capacity from minority communities to the state apparatus. The state gains administrative control over vakifs, appointment authority over religious leadership, and property rights over communal assets. Minority communities lose the vehicles for intergenerational continuity.
% ABSENT_VOICES: Minority religious institutions themselves (patriarchates, chief rabbinate, Armenian patriarchate) — they would object to denial of legal personality and property rights but have no formal seat in Turkish domestic interpretation. Guarantor states — excluded by sovereignty claim. Would-be minority clergy — foreclosed by closure of theological schools.
% DISAPPEARANCE_RATIONALE: If the restrictive reading vanished overnight, minority institutions would immediately claim legal personality, property restitution, and right to operate theological schools under Lausanne. The Turkish state would face treaty obligation to recognize institutional autonomy. Property regimes, educational licensing, and vakif administration would reorganize around minority self-administration. The world rearranges substantially.
% FOUNDING_PROBLEM: Post-WWI settlement needed to protect Ottoman minority populations in new Turkish Republic while accommodating Turkish sovereignty demands. Lausanne Treaty (1923) replaced Sevres Treaty's robust minority protections with a narrower framework. The founding problem was: how to guarantee minority survival without creating foreign-protected enclaves within Turkish territory.
% FOUNDING_PROBLEM_CORROBORATION: Turkish state attests founding problem was solved by guaranteeing individual worship within sovereign equality. Minority institutions and guarantor-state diplomatic archives attest the problem was minority institutional survival — which remains live as institutions continue to atrophy. International legal scholarship (e.g., Hofmann, Alexandris, Tsitselikis) corroborates that Lausanne's minority regime was designed for functional continuity, not merely individual rights.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__restrictive_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__restrictive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__restrictive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(lausanne_minority_protections__restrictive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__restrictive_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is high (0.82) because the constraint transfers nearly all institutional capacity — legal personality, property control, clergy formation — from minorities to the state. Suppression is very high (0.88) because persistence depends on active enforcement: courts denying legal personality, vakif boards seizing properties, education law closing seminaries. Theater ratio (0.45) reflects the genuine individual-worship floor (real coordination) overlaying the extractive institutional denial. Accessibility collapse (0.72) is high but not total: ECHR jurisprudence and EU accession process created partial alternative pathways (individual applications, some property returns via 2008/2011 foundations law amendments), but these are narrow and discretionary. Resistance (0.65) is substantial: minority communities pursue legal strategies, international advocacy, and institutional persistence despite foreclosure.
 *
 * PERSPECTIVAL GAP:
 *   From the state seat, the arrangement is a solved coordination problem: individual worship guaranteed, sovereignty preserved. From minority institution seats, the same structure is enforced extraction: their institutional vehicles for intergenerational continuity are legally foreclosed. The engine computes this divergence from structural data — the state's arbitrage-grade exit vs. minorities' identity-locked/trapped exit drives the χ split.
 *
 * DIRECTIONALITY LOGIC:
 *   Turkish state apparatus is agenda_setter and beneficiary: sets interpretation, enforces it, collects institutional control (d ~ 0.1). Minority institutions, theological education, and property holders are payers: bear extraction with identity-locked or trapped exit (d ~ 0.9). Individual believers are dual: beneficiaries of worship floor but payers of institutional collapse (d ~ 0.5). Guarantor states are excluded: would object but structurally locked out (d not computed). ECHR and scholars are observers (d = 0.5 analytical).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (minority survival without foreign enclaves) is contested: state says solved, minorities say live. The restrictive reading persists not because it solves the founding problem but because it extracts institutional capacity for the state. Mandatrophy is unresolved: the arrangement's mandate (minority protection) has atrophied into a vehicle for state consolidation. The constraint is a snare, not a piton, because the state actively benefits and enforces — it is not inertially maintained.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_text_ambiguity,
    'Does the Lausanne Treaty text (Articles 38-44) structurally support only individual worship guarantees, or does ''institutions'' and ''religious freedom'' language entail institutional autonomy?',
    'Comparative analysis of French/English treaty texts, drafting history (procès-verbals), and contemporaneous understanding of ''minority protections'' in 1923 international law.',
    'If text supports institutional reading, restrictive reading is a constructed narrowing (snare confirmed). If text is genuinely limited to individual rights, restrictive reading may be a rope with later extractive layering.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_text_ambiguity, conceptual, 'Whether treaty text ambiguity is genuine or constructed to enable restrictive interpretation.').

omega_variable(
    state_extraction_vs_sovereignty,
    'Is the state''s restrictive interpretation driven by genuine sovereignty concerns or by extraction of minority institutional assets?',
    'Analyze correlation between restrictive interpretation milestones (1936 vakif law, 1971 Halki closure, property seizures) and state acquisition of minority assets. Compare with majority institutions'' treatment.',
    'If extraction-driven, snare classification solidifies. If sovereignty-driven with incidental extraction, tangled_rope possible (coordination: sovereign equality; extraction: minority assets).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_extraction_vs_sovereignty, empirical, 'Motivational structure behind the restrictive reading''s persistence.').

omega_variable(
    kernel_reading_fork,
    'Is this constraint one reading of a contested kernel (Lausanne minority protections), or a standalone constraint that happens to cite Lausanne?',
    'Structural comparison: do all three readings (restrictive, expansive, guarantor) share the same referent (Treaty Articles 38-44) but instantiate different ε, beneficiaries, victims? If yes, kernel frame applies.',
    'Confirms kernel structure — this reading''s ε is assessed against the standing arrangement (Turkish state practice), not the reading''s endorsed alternative. Prevents ε-referent confusion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_fork, conceptual, 'Whether the kernel/reading decomposition correctly models the structural situation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__restrictive_reading, 1923, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t1923, lausanne_minority_protections__restrictive_reading, theater_ratio, 1923, 0.15).
narrative_ontology:measurement(laus_tr_t1935, lausanne_minority_protections__restrictive_reading, theater_ratio, 1935, 0.25).
narrative_ontology:measurement(laus_tr_t1955, lausanne_minority_protections__restrictive_reading, theater_ratio, 1955, 0.35).
narrative_ontology:measurement(laus_tr_t1971, lausanne_minority_protections__restrictive_reading, theater_ratio, 1971, 0.4).
narrative_ontology:measurement(laus_tr_t1999, lausanne_minority_protections__restrictive_reading, theater_ratio, 1999, 0.42).
narrative_ontology:measurement(laus_tr_t2011, lausanne_minority_protections__restrictive_reading, theater_ratio, 2011, 0.44).
narrative_ontology:measurement(laus_tr_t2024, lausanne_minority_protections__restrictive_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(laus_be_t1923, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1923, 0.35).
narrative_ontology:measurement(laus_be_t1935, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1935, 0.55).
narrative_ontology:measurement(laus_be_t1955, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1955, 0.65).
narrative_ontology:measurement(laus_be_t1971, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1971, 0.72).
narrative_ontology:measurement(laus_be_t1999, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1999, 0.78).
narrative_ontology:measurement(laus_be_t2011, lausanne_minority_protections__restrictive_reading, base_extractiveness, 2011, 0.8).
narrative_ontology:measurement(laus_be_t2024, lausanne_minority_protections__restrictive_reading, base_extractiveness, 2024, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(laus_su_t1923, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1923, 0.6).
narrative_ontology:measurement(laus_su_t1935, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1935, 0.75).
narrative_ontology:measurement(laus_su_t1955, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1955, 0.8).
narrative_ontology:measurement(laus_su_t1971, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1971, 0.85).
narrative_ontology:measurement(laus_su_t1999, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1999, 0.87).
narrative_ontology:measurement(laus_su_t2011, lausanne_minority_protections__restrictive_reading, suppression_requirement, 2011, 0.88).
narrative_ontology:measurement(laus_su_t2024, lausanne_minority_protections__restrictive_reading, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__restrictive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(lausanne_minority_protections__restrictive_reading, 0.12).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, turkish_vakif_law).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, turkish_education_law).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, echr_minority_jurisprudence).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, eu_accession_criteria_turkey).

% DUAL FORMULATION NOTE:
% Part of Lausanne kernel family: restrictive_reading (this) forecloses expansive_reading; coexists_with guarantor_reading. All three share kernel lausanne_minority_protections but instantiate different constraints with different ε, beneficiary/victim structures, and types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lausanne_minority_protections__restrictive_reading, institutional, 0.1).
constraint_indexing:directionality_override(lausanne_minority_protections__restrictive_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
