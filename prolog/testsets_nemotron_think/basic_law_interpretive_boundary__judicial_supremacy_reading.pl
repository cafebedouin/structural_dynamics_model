% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__judicial_supremacy_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__judicial_supremacy_reading
 *   human_readable: Basic Laws as Higher-Order Framework with Judicial Supremacy
 *   domain: constitutional_law/comparative_constitutionalism/judicial_review_theory
 *
 * SUMMARY:
 *   This constraint story captures the judicial supremacy reading of Israel's
 *   constitutional order: the Supreme Court's 1995 assertion (Mizrahi Bank)
 *   that Basic Laws constitute a higher-order legal framework it must
 *   interpret and enforce, with binding invalidation of contradictory Knesset
 *   legislation. The reading claims this arrangement coordinates rights
 *   protection against majoritarian excess; structurally it also extracts
 *   final interpretive authority from the elected legislature. The constraint
 *   has intensified over three decades as the Court expanded justiciability,
 *   reasonableness review, and Basic Law amendment scrutiny — provoking
 *   escalating political resistance culminating in the 2023 judicial reform
 *   crisis.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.72).
domain_priors:suppression_score(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.78).
domain_priors:theater_ratio(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__judicial_supremacy_reading, "Basic Laws as Higher-Order Framework with Judicial Supremacy").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__judicial_supremacy_reading, "constitutional_law/comparative_constitutionalism/judicial_review_theory").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__judicial_supremacy_reading, '6dd63228-3139-4bfe-a000-f949e254544b').
narrative_ontology:cs_kernel_codification('6dd63228-3139-4bfe-a000-f949e254544b', formalized).
narrative_ontology:cs_authority_grounding('6dd63228-3139-4bfe-a000-f949e254544b', lineage).
narrative_ontology:cs_interpretation_layer_present('6dd63228-3139-4bfe-a000-f949e254544b').
narrative_ontology:cs_reading_relation('6dd63228-3139-4bfe-a000-f949e254544b', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('6dd63228-3139-4bfe-a000-f949e254544b', basic_law_interpretive_boundary__balanced_contestation_reading, coexists_with).
narrative_ontology:cs_axiom('6dd63228-3139-4bfe-a000-f949e254544b', foundational, judicial_exclusive_interpretive_authority).
narrative_ontology:cs_axiom_status(judicial_exclusive_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('6dd63228-3139-4bfe-a000-f949e254544b', judicial_exclusive_interpretive_authority, conventional).
narrative_ontology:cs_axiom('6dd63228-3139-4bfe-a000-f949e254544b', foundational, basic_laws_as_superior_law).
narrative_ontology:cs_axiom_status(basic_laws_as_superior_law, holdable).
narrative_ontology:cs_axiom_grounding('6dd63228-3139-4bfe-a000-f949e254544b', basic_laws_as_superior_law, conventional).
narrative_ontology:cs_reference_frame('6dd63228-3139-4bfe-a000-f949e254544b', constitutional_revolution_framework).
narrative_ontology:cs_drift_state('6dd63228-3139-4bfe-a000-f949e254544b', contemporary_judicial_reform_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6dd63228-3139-4bfe-a000-f949e254544b', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, rights_claimants).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court_institution).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset_legislative_autonomy).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, majority_coalitions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, government_executive).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__judicial_supremacy_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__judicial_supremacy_reading, judicial_review_as_rights_protection).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_laws_entrenched_status).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Basic Laws as superior constitutional norms; exercises binding judicial review over Knesset legislation and executive action; institutional legitimacy and authority depend on maintaining this interpretive monopoly; can shape doctrine incrementally through case law.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court, agenda_setter,
    institutional, generational, arbitrage, national).

% Elected legislature whose legislation is subject to invalidation by Supreme Court; can amend Basic Laws by simple majority but faces judicial scrutiny of amendment legitimacy; political costs of override are high; constrained by coalition dynamics and international legitimacy concerns.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset, payer,
    institutional, biographical, constrained, national).

% Individuals and groups (minorities, NGOs, marginalized communities) who petition Court to invalidate rights-violating legislation; gain veto power over majoritarian decisions through litigation; dependent on Court's willingness to hear cases and expand rights doctrines; no alternative forum for constitutional claims.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, rights_claimants, beneficiary,
    organized, biographical, constrained, national).

% Executive branch subject to judicial review of administrative decisions and statutory authority; must comply with Court orders even when politically costly; can influence judicial appointments but cannot directly override invalidation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, government_executive, payer,
    powerful, immediate, constrained, national).

% Political parties, legal scholars, and civil society actors who argue Knesset should have final interpretive authority over Basic Laws; structurally excluded from interpretive role by Court's self-arrogated supremacy; can only pursue legislative override or constitutional reform.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, parliamentary_sovereignty_advocates, excluded,
    organized, biographical, trapped, national).

% Comparative constitutional scholars, foreign courts, international organizations analyzing Israel's unique uncodified constitutionalism; provide external legitimacy or criticism but no enforcement power.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, international_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a binding mechanism to protect fundamental rights against legislative majorities in a system lacking a formal entrenched constitution; coordinates expectations about rights protection across political cycles.
% TRANSFER_FUNCTION: Moves final interpretive authority over constitutional norms from the elected legislature (Knesset) to the unelected Supreme Court; transfers veto power over legislation from majority coalitions to rights-claimants via litigation.
% ABSENT_VOICES: Parliamentary sovereignty advocates and voters who prefer majoritarian democratic control over constitutional interpretation are structurally excluded from the interpretive role; their preference for legislative finality is overridden by Court's self-declared authority.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished overnight, the Knesset would regain unchecked legislative power over Basic Laws; rights-claimants would lose their constitutional veto; the entire post-1992 constitutional architecture would collapse into pure parliamentary sovereignty.
% FOUNDING_PROBLEM: Israel lacked a formal written constitution; the 1992 Basic Laws (Human Dignity, Freedom of Occupation) were enacted as constitutional building blocks but contained no explicit judicial review clause; the Court filled this gap by declaring itself the authoritative interpreter in the 1995 Mizrahi Bank decision.
% FOUNDING_PROBLEM_CORROBORATION: Comparative constitutional scholars (e.g., Hirschl, Ginsburg) document the Court's self-empowerment; Israeli legal academia outside Court institutional interest (e.g., Rubinstein, Navot) corroborate the founding gap but dispute the judicial supremacy solution; no corroboration from Court beneficiaries alone.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) reflects the Court's power to nullify legislation on open-textured rights grounds — a substantial transfer from legislature to judiciary. Suppression (0.78) is high because the Knesset cannot easily exit: override requires political consensus the Court's rulings often disrupt, and international legitimacy costs of explicit override are severe. Theater is low (0.22) — the Court actively decides cases, develops doctrine, and enforces orders. Accessibility collapse (0.75) is high because legislative alternatives (override clauses, constitutional amendment) face near-insurmountable political and institutional barriers. Resistance (0.68) is substantial and growing, evidenced by repeated legislative override attempts and the 2023 reform drive.
 *
 * PERSPECTIVAL GAP:
 *   From the Court's seat (agenda_setter, institutional power, arbitrage exit), the constraint appears as genuine coordination — a necessary constitutional innovation. From the Knesset's seat (payer, institutional power, constrained exit), it appears as extraction — an unelected body seizing sovereign authority. From rights-claimants' seat (beneficiary, organized power, constrained exit), it appears as essential protection. The engine computes this divergence from the declared structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The Court is the structural beneficiary (d ~ 0.15): it gains institutional authority, agenda control, and legitimacy as constitutional guardian. Rights-claimants are secondary beneficiaries (d ~ 0.3): they gain effective veto but depend on Court's doctrinal choices. The Knesset is primary target (d ~ 0.85): it bears the constraint's full extractive force with constrained exit — it cannot credibly commit to rights protection without Court, but Court's power is unbounded by text. Government executive sits near Knesset (d ~ 0.75). Parliamentary sovereignty advocates are excluded (d ~ 0.95): their interpretive role is foreclosed by the constraint itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (rights protection without formal constitution) remains live but contested. The arrangement has not atrophied — its function has expanded. Mandatrophy risk is low for this reading; the Court actively maintains and extends its authority. The mandatrophy question applies more to the parliamentary sovereignty reading (dead founding problem: legislative supremacy persists without judicial veto) or balanced contestation (contested founding problem: both institutions claim legitimate but bounded authority).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_naturalness_ambiguity,
    'Is the judicial supremacy arrangement a natural entailment of the Basic Laws'' constitutional status, or a constructed power grab by the Court?',
    'Historical analysis of 1992 legislative intent; comparative study of constitutional courts'' self-empowerment patterns; examination of whether alternative interpretive frameworks were politically viable.',
    'If natural entailment, the constraint approaches Mountain status (low ε); if constructed, it is Tangled Rope or Snare with high ε for legislative autonomy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutional_naturalness_ambiguity, conceptual, 'Whether judicial supremacy derives from constitutional text or judicial creation.').

omega_variable(
    rights_protection_necessity,
    'Is judicial supremacy structurally necessary for effective rights protection, or does it extract legislative power beyond what rights protection requires?',
    'Counterfactual analysis of rights outcomes under alternative models (UK-style parliamentary sovereignty with political rights culture; Canadian-style dialogic review); empirical assessment of Knesset rights compliance without judicial veto.',
    'If necessary, extraction is coordination cost (Rope/Tangled Rope); if excessive, extraction is rent (Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rights_protection_necessity, empirical, 'Whether the extraction of legislative power is proportional to the coordination benefit.').

omega_variable(
    suppression_mechanism_composition,
    'Is the constraint''s suppression primarily structural (formal invalidation power) or internalized (legislative self-censorship and anticipatory compliance)?',
    'Analysis of legislative drafting practices: track bills withdrawn or amended preemptively due to anticipated judicial review vs. bills actually invalidated; interview legislators and legal advisors.',
    'If internalized suppression dominates, effective suppression exceeds formal invalidation rate; the constraint''s reach extends beyond its formal enforcement actions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_composition, empirical, 'Structural vs. internalized suppression in legislative-judicial relations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__judicial_supremacy_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t1992, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 1992, 0.1).
narrative_ontology:measurement(basi_tr_t1995, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 1995, 0.12).
narrative_ontology:measurement(basi_tr_t2000, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(basi_tr_t2006, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2006, 0.18).
narrative_ontology:measurement(basi_tr_t2015, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(basi_tr_t2023, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2023, 0.22).

% Extraction over time
narrative_ontology:measurement(basi_be_t1992, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 1992, 0.25).
narrative_ontology:measurement(basi_be_t1995, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 1995, 0.45).
narrative_ontology:measurement(basi_be_t2000, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(basi_be_t2006, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2006, 0.62).
narrative_ontology:measurement(basi_be_t2015, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement(basi_be_t2023, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2023, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t1992, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 1992, 0.3).
narrative_ontology:measurement(basi_su_t1995, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 1995, 0.55).
narrative_ontology:measurement(basi_su_t2000, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement(basi_su_t2006, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2006, 0.7).
narrative_ontology:measurement(basi_su_t2015, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2015, 0.75).
narrative_ontology:measurement(basi_su_t2023, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2023, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.1).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_amendment_procedure).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset_override_clause_proposals).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, judicial_appointments_process).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, reasonableness_doctrine_scope).

% DUAL FORMULATION NOTE:
% This reading and parliamentary_sovereignty_reading are mutually exclusive in any single constitutional framework (forecloses relation). Both coexist with balanced_contestation_reading as live positions in Israeli constitutional discourse. The ε values diverge sharply: judicial supremacy reading authors high ε for legislative autonomy; parliamentary sovereignty reading authors high ε for rights protection (absent judicial veto).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(basic_law_interpretive_boundary__judicial_supremacy_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
