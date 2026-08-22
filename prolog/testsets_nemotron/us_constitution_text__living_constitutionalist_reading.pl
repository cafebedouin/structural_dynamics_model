% ============================================================================
% CONSTRAINT STORY: us_constitution_text__living_constitutionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__living_constitutionalist_reading, []).

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
 *   constraint_id: us_constitution_text__living_constitutionalist_reading
 *   human_readable: Living Constitutionalist Reading of the US Constitution
 *   domain: legal/constitutional
 *
 * SUMMARY:
 *   The living constitutionalist reading treats the Constitution as a
 *   framework whose principles must be adapted to contemporary social
 *   conditions. It emerged from the recognition that the 1789 text cannot
 *   directly govern modern circumstances (industrial economy, digital
 *   surveillance, reproductive technology) and that rigid adherence to
 *   original understanding would produce results the framers could not have
 *   intended. The constraint operates through judicial interpretation: courts
 *   identify principles in the text (equal protection, due process, cruel and
 *   unusual punishment) and apply them to present conditions using
 *   post-ratification practice, evolving social consensus, and institutional
 *   experience as authoritative guides. This reading has been the dominant
 *   mode in US constitutional law since the mid-20th century, though its
 *   dominance has been contested since the 1970s.
 *
 * KEY AGENTS:
 *   - rights_claimants_in_changed_contexts: Primary beneficiary (moderate/constrained) — gains recognition of rights in circumstances the framers did not anticipate
 *   - adaptive_judiciary: Agenda setter (institutional/biographical) — empowered to adapt principles, derives institutional authority from the interpretive role
 *   - claims_to_fixed_meaning_as_democratic_constraint: Victim (organized/constrained) — originalist and textualist arguments are structurally disadvantaged in the adaptive framework
 *   - constitutional_scholars: Observer (analytical/analytical) — analyze and legitimate the adaptive framework
 *   - elected_branches: Secondary actor (institutional/biographical) — constrained by adaptive judicial review but also benefit from constitutional flexibility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__living_constitutionalist_reading, 0.15).
domain_priors:suppression_score(us_constitution_text__living_constitutionalist_reading, 0.08).
domain_priors:theater_ratio(us_constitution_text__living_constitutionalist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__living_constitutionalist_reading, rope).
narrative_ontology:human_readable(us_constitution_text__living_constitutionalist_reading, "Living Constitutionalist Reading of the US Constitution").
narrative_ontology:topic_domain(us_constitution_text__living_constitutionalist_reading, "legal/constitutional").

domain_priors:requires_active_enforcement(us_constitution_text__living_constitutionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__living_constitutionalist_reading, '25e278e0-1ed5-42b5-9bd5-dfb6529bf396').
narrative_ontology:cs_kernel_codification('25e278e0-1ed5-42b5-9bd5-dfb6529bf396', fixed_text).
narrative_ontology:cs_authority_grounding('25e278e0-1ed5-42b5-9bd5-dfb6529bf396', lineage).
narrative_ontology:cs_interpretation_layer_present('25e278e0-1ed5-42b5-9bd5-dfb6529bf396').
narrative_ontology:cs_reading_relation('25e278e0-1ed5-42b5-9bd5-dfb6529bf396', us_constitution_text__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('25e278e0-1ed5-42b5-9bd5-dfb6529bf396', us_constitution_text__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('25e278e0-1ed5-42b5-9bd5-dfb6529bf396', foundational, constitutional_principles_require_adaptation).
narrative_ontology:cs_axiom_status(constitutional_principles_require_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('25e278e0-1ed5-42b5-9bd5-dfb6529bf396', constitutional_principles_require_adaptation, instrumental).
narrative_ontology:cs_axiom('25e278e0-1ed5-42b5-9bd5-dfb6529bf396', foundational, post_ratification_practice_is_authoritative).
narrative_ontology:cs_axiom_status(post_ratification_practice_is_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('25e278e0-1ed5-42b5-9bd5-dfb6529bf396', post_ratification_practice_is_authoritative, conventional).
narrative_ontology:cs_reference_frame('25e278e0-1ed5-42b5-9bd5-dfb6529bf396', founding_generation_understanding).
narrative_ontology:cs_drift_state('25e278e0-1ed5-42b5-9bd5-dfb6529bf396', contemporary_adaptive_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('25e278e0-1ed5-42b5-9bd5-dfb6529bf396', '').
narrative_ontology:cs_kernel_id(us_constitution_text__living_constitutionalist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__living_constitutionalist_reading, rights_claimants_in_changed_contexts).
narrative_ontology:constraint_beneficiary(us_constitution_text__living_constitutionalist_reading, adaptive_judiciary).
narrative_ontology:constraint_victim(us_constitution_text__living_constitutionalist_reading, claims_to_fixed_meaning_as_democratic_constraint).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_text__living_constitutionalist_reading, elected_branches).
narrative_ontology:constraint_victim(us_constitution_text__living_constitutionalist_reading, elected_branches).
narrative_ontology:constraint_vindicates(us_constitution_text__living_constitutionalist_reading, constitutional_principles_require_adaptation_to_contemporary_circumstances).
narrative_ontology:constraint_vindicates(us_constitution_text__living_constitutionalist_reading, post_ratification_practice_is_authoritative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls constitutional interpretation through judicial review; derives institutional legitimacy and authority from the adaptive framework; lifetime tenure and professional consensus make exit nearly costless. Adapts principles to contemporary circumstances using post-ratification practice and social consensus as guides.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, adaptive_judiciary, agenda_setter,
    institutional, biographical, arbitrage, national).

% Groups seeking constitutional recognition of rights in circumstances the framers did not anticipate (e.g., abortion access, same-sex marriage, digital privacy, transgender rights). They gain protections through adaptive interpretation but depend on judicial willingness to extend principles. Their exit is constrained — they cannot easily create alternative rights-recognition mechanisms.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, rights_claimants_in_changed_contexts, beneficiary,
    moderate, biographical, constrained, national).

% Originalist and textualist arguments that constitutional meaning is fixed at ratification and serves as a democratic constraint on judicial power. These claims are structurally disadvantaged in the adaptive framework — they must win within a system that treats adaptation as authoritative. They have built institutional counter-infrastructure (Federalist Society, originalist judges, scholarly networks) but face constrained exit: leaving the adaptive framework means abandoning the dominant interpretive culture.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, claims_to_fixed_meaning_as_democratic_constraint, payer,
    organized, generational, constrained, national).

% Analyze, legitimate, and contest the adaptive framework. They produce the theoretical architecture that makes adaptive interpretation appear coherent and principled rather than arbitrary. Their exit is analytical — they can adopt any interpretive stance without material cost.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, constitutional_scholars, observer,
    analytical, generational, analytical, universal).

% Congress and the presidency are constrained by adaptive judicial review (payer) but also benefit from constitutional flexibility that avoids amendment gridlock (beneficiary). They can respond to judicial decisions through legislation, appointments, or (rarely) constitutional amendment. Their exit is constrained by the constitutional structure itself.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, elected_branches, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_text__living_constitutionalist_reading, elected_branches, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for applying fixed constitutional text to changing social conditions without requiring constant formal amendment. Solves the temporal gap problem: how an 18th-century document governs a 21st-century society.
% TRANSFER_FUNCTION: Moves interpretive authority from fixed historical meaning to contemporary judicial judgment. Transfers the power to define constitutional boundaries from the ratifying generation to the current judiciary, mediated by post-ratification practice and social consensus.
% ABSENT_VOICES: The ratifying generation (dead, cannot object); future generations (not yet present, will inherit the adaptive precedents); citizens who believe constitutional change should come only through Article V amendment (structurally excluded from interpretive authority by the adaptive framework itself).
% DISAPPEARANCE_RATIONALE: If the living constitutionalist reading vanished overnight, constitutional law would revert to originalist or textualist modes. Rights recognized only through adaptive interpretation (substantive due process privacy rights, equal protection extensions to gender/sexual orientation, modern Fourth Amendment applications) would lose their doctrinal foundation. The Constitution would become a static document requiring formal amendment for each new circumstance.
% FOUNDING_PROBLEM: The 1789 Constitution cannot directly address modern governance problems (industrial regulation, digital surveillance, reproductive technology, climate change, etc.) without a mechanism for principled adaptation. The Article V amendment process is too cumbersome for continuous adaptation.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the framers themselves (Madison in Federalist 37 on the need for 'liquidation' of meaning through practice), by early Supreme Court justices (Marshall in McCulloch v. Maryland on the Constitution as a framework 'intended to endure for ages to come'), and by contemporary scholars across the ideological spectrum who agree that some adaptation mechanism is necessary — they disagree only on its scope and authorization.
narrative_ontology:disappearance_verdict(us_constitution_text__living_constitutionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__living_constitutionalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__living_constitutionalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(us_constitution_text__living_constitutionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__living_constitutionalist_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__living_constitutionalist_reading_tests).
:- end_tests(us_constitution_text__living_constitutionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the constraint primarily coordinates: it provides a mechanism for applying 18th-century principles to 21st-century problems without constant amendment. The declared beneficiaries (rights claimants in changed contexts) receive genuine coordination value — the constraint solves a real problem of temporal gap. Suppression is low (0.08) because originalist arguments remain legally cognizable, are heard in courts, and have won major cases (e.g., Heller, Dobbs, Bruen); they are disadvantaged but not excluded. Theater ratio is moderate (0.25) because some adaptive decisions appear driven more by judicial policy preference than by measurable social change, creating performative elements. Accessibility collapse is moderate (0.35) because fixed-meaning alternatives remain live in public discourse and judicial practice. Resistance is moderate (0.45) because the originalist counter-movement has built institutional infrastructure (Federalist Society, originalist judges, scholarly networks) that actively contests the adaptive framework.
 *
 * PERSPECTIVAL GAP:
 *   From the adaptive judiciary's seat, the constraint is genuine coordination — it empowers them to keep the Constitution functional. From rights claimants' seat, it is a Rope — they gain protections that would not exist under fixed meaning. From originalist advocates' seat, it operates as extraction — their interpretive method is disadvantaged by a framework that treats its own adaptiveness as authoritative. The engine computes these divergences from the structural data: the agenda_setter (judiciary) has institutional power and arbitrage exit; the payer (fixed-meaning claims) has organized power but constrained exit; the beneficiary (rights claimants) has moderate power and constrained exit.
 *
 * DIRECTIONALITY LOGIC:
 *   The adaptive judiciary is the structural beneficiary (d near 0.0): they control the interpretive framework, derive institutional authority from it, and face minimal exit costs (lifetime tenure, professional consensus). Rights claimants in changed contexts are beneficiaries (d ~0.2): they gain rights recognition but depend on judicial goodwill. Claims to fixed meaning are victims (d ~0.8): their interpretive framework is structurally disadvantaged, though not excluded — they must win within a system that treats adaptation as the default. Elected branches sit near symmetric (d ~0.5): constrained by judicial review but also freed from amendment gridlock.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (governing modern conditions with an 18th-century text) remains live — social change continues to create gaps between text and circumstance. The constraint has not atrophied; its coordination function is ongoing. However, the rising theater ratio suggests a drift: as the adaptive framework matures, the gap between 'adaptation to social change' and 'judicial policy preference' narrows, creating mandatrophy risk. The originalist counter-movement is precisely the detection of this drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine coordination mechanism for adapting constitutional principles, or does it serve as a cover for judicial policymaking beyond the constitutional text?',
    'Longitudinal analysis of whether adaptive interpretations systematically track measurable social change or track judicial policy preferences; cross-jurisdictional comparison with fixed-meaning frameworks.',
    'If the latter, the constraint''s extractiveness is understated — it would operate as a Tangled Rope where the coordination function (adaptation) is real but asymmetric extraction (judicial preference imposition) coexists. If the former, it remains a Rope with low extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the living constitutionalist reading is a pure coordination mechanism or a hybrid with extraction').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of fixed-meaning claims structural (institutional exclusion from interpretive authority) or internalized (practitioners accepting adaptive interpretation as the only legitimate mode)?',
    'Survey of constitutional law curriculum, judicial appointment criteria, and scholarly citation patterns to measure whether originalist arguments are structurally excluded or internally marginalized.',
    'If internalized, effective suppression of fixed-meaning claims is higher than the structural measure suggests — the constraint carries its own suppression mechanism through professional socialization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of competing interpretive frameworks').

omega_variable(
    beneficiary_boundary,
    'Do rights claimants in changed social contexts genuinely benefit from adaptive interpretation, or does the reading primarily benefit the institutional actors who administer it (judges, scholars, advocacy organizations)?',
    'Track win rates and doctrinal trajectories for rights claims under adaptive vs. fixed-meaning frameworks; measure career advancement patterns for advocates of each reading.',
    'If institutional actors are the primary beneficiaries, the constraint shifts toward Tangled Rope or Snare — the declared beneficiary class becomes a legitimating cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_boundary, empirical, 'Whether declared beneficiaries are actual beneficiaries or legitimating cover').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__living_constitutionalist_reading, 1789, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1789, us_constitution_text__living_constitutionalist_reading, theater_ratio, 1789, 0.1).
narrative_ontology:measurement(us_c_tr_t1868, us_constitution_text__living_constitutionalist_reading, theater_ratio, 1868, 0.15).
narrative_ontology:measurement(us_c_tr_t1937, us_constitution_text__living_constitutionalist_reading, theater_ratio, 1937, 0.2).
narrative_ontology:measurement(us_c_tr_t1954, us_constitution_text__living_constitutionalist_reading, theater_ratio, 1954, 0.22).
narrative_ontology:measurement(us_c_tr_t1973, us_constitution_text__living_constitutionalist_reading, theater_ratio, 1973, 0.25).
narrative_ontology:measurement(us_c_tr_t2015, us_constitution_text__living_constitutionalist_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(us_c_tr_t2022, us_constitution_text__living_constitutionalist_reading, theater_ratio, 2022, 0.25).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1789, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 1789, 0.05).
narrative_ontology:measurement(us_c_be_t1868, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 1868, 0.08).
narrative_ontology:measurement(us_c_be_t1937, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 1937, 0.12).
narrative_ontology:measurement(us_c_be_t1954, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 1954, 0.15).
narrative_ontology:measurement(us_c_be_t1973, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 1973, 0.18).
narrative_ontology:measurement(us_c_be_t2015, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 2015, 0.15).
narrative_ontology:measurement(us_c_be_t2022, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 2022, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1789, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 1789, 0.02).
narrative_ontology:measurement(us_c_su_t1868, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 1868, 0.04).
narrative_ontology:measurement(us_c_su_t1937, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 1937, 0.06).
narrative_ontology:measurement(us_c_su_t1954, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 1954, 0.08).
narrative_ontology:measurement(us_c_su_t1973, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 1973, 0.1).
narrative_ontology:measurement(us_c_su_t2015, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 2015, 0.08).
narrative_ontology:measurement(us_c_su_t2022, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 2022, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__living_constitutionalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_text__living_constitutionalist_reading, 0.1).
narrative_ontology:affects_constraint(us_constitution_text__living_constitutionalist_reading, us_constitution_text__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__living_constitutionalist_reading, us_constitution_text__positivist_reading).
narrative_ontology:affects_constraint(us_constitution_text__living_constitutionalist_reading, judicial_review_power).
narrative_ontology:affects_constraint(us_constitution_text__living_constitutionalist_reading, substantive_due_process_doctrine).
narrative_ontology:affects_constraint(us_constitution_text__living_constitutionalist_reading, equal_protection_jurisprudence).

% DUAL FORMULATION NOTE:
% This constraint and its siblings form the us_constitution_text constraint family. The living reading has lower extractiveness (0.15) because it coordinates adaptation; the originalist reading has higher extractiveness (~0.35) because it suppresses adaptive claims; the positivist reading has moderate extractiveness (~0.25) because it displaces moral reasoning with procedural validity. They share the same kernel but author different structural relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_text__living_constitutionalist_reading, institutional, 0.05).
constraint_indexing:directionality_override(us_constitution_text__living_constitutionalist_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
