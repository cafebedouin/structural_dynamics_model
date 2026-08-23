% ============================================================================
% CONSTRAINT STORY: salic_prohibition__sovereign_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__sovereign_override_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: salic_prohibition__sovereign_override_reading
 *   human_readable: Salic Law Prohibition on Female Succession (Sovereign Override Reading)
 *   domain: constitutional/dynastic/political_history
 *
 * SUMMARY:
 *   The Salic Law prohibition on female succession, originating in the
 *   Frankish Lex Salica (c. 500), was invoked by the Capetian and Valois
 *   kings of France (1316, 1328) to exclude female-line claimants (Joan II of
 *   Navarre, Edward III of England). This reading — the
 *   sovereign_override_reading — treats the prohibition as positive law
 *   enacted and maintained by sovereign authority, revocable by a sovereign
 *   legislative act (Pragmatic Sanction). Under this reading, Charles VI's
 *   1713 Pragmatic Sanction legitimately overridden the Salic Law to secure
 *   his daughter Maria Theresa's succession; challengers (Bavaria, Saxony,
 *   Prussia, Spain) were rebels against legitimate authority, and the ensuing
 *   War of Austrian Succession (1740-1748) was a defensive war to protect
 *   dynastic continuity. The constraint is thus a Tangled Rope: it
 *   coordinates succession stability (genuine coordination) but extracts the
 *   throne from female heirs (asymmetric extraction), requires active
 *   enforcement (parlements, estates, military), and its persistence depends
 *   on sovereign will — not natural law.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__sovereign_override_reading, 0.65).
domain_priors:suppression_score(salic_prohibition__sovereign_override_reading, 0.75).
domain_priors:theater_ratio(salic_prohibition__sovereign_override_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__sovereign_override_reading, tangled_rope).
narrative_ontology:human_readable(salic_prohibition__sovereign_override_reading, "Salic Law Prohibition on Female Succession (Sovereign Override Reading)").
narrative_ontology:topic_domain(salic_prohibition__sovereign_override_reading, "constitutional/dynastic/political_history").

domain_priors:requires_active_enforcement(salic_prohibition__sovereign_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__sovereign_override_reading, '3c2ba4a7-e820-47d1-853a-49b8f46ca7bf').
narrative_ontology:cs_kernel_codification('3c2ba4a7-e820-47d1-853a-49b8f46ca7bf', formalized).
narrative_ontology:cs_authority_grounding('3c2ba4a7-e820-47d1-853a-49b8f46ca7bf', lineage).
narrative_ontology:cs_interpretation_layer_present('3c2ba4a7-e820-47d1-853a-49b8f46ca7bf').
narrative_ontology:cs_reading_relation('3c2ba4a7-e820-47d1-853a-49b8f46ca7bf', salic_prohibition__immutable_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('3c2ba4a7-e820-47d1-853a-49b8f46ca7bf', salic_prohibition__cognatic_reversion_reading, coexists_with).
narrative_ontology:cs_axiom('3c2ba4a7-e820-47d1-853a-49b8f46ca7bf', foundational, sovereign_legislative_supremacy_over_succession).
narrative_ontology:cs_axiom_status(sovereign_legislative_supremacy_over_succession, holdable).
narrative_ontology:cs_axiom_grounding('3c2ba4a7-e820-47d1-853a-49b8f46ca7bf', sovereign_legislative_supremacy_over_succession, conventional).
narrative_ontology:cs_axiom('3c2ba4a7-e820-47d1-853a-49b8f46ca7bf', secondary, pragmatic_sanction_as_valid_override).
narrative_ontology:cs_axiom_status(pragmatic_sanction_as_valid_override, holdable).
narrative_ontology:cs_axiom_grounding('3c2ba4a7-e820-47d1-853a-49b8f46ca7bf', pragmatic_sanction_as_valid_override, conventional).
narrative_ontology:cs_reference_frame('3c2ba4a7-e820-47d1-853a-49b8f46ca7bf', sovereign_legislative_authority).
narrative_ontology:cs_drift_state('3c2ba4a7-e820-47d1-853a-49b8f46ca7bf', pragmatic_sanction_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3c2ba4a7-e820-47d1-853a-49b8f46ca7bf', '').
narrative_ontology:cs_kernel_id(salic_prohibition__sovereign_override_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, male_dynastic_heirs).
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, nobility_estates).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, excluded_female_heirs).
narrative_ontology:constraint_vindicates(salic_prohibition__sovereign_override_reading, sovereign_legislative_supremacy).
narrative_ontology:constraint_vindicates(salic_prohibition__sovereign_override_reading, positive_law_revocability).
narrative_ontology:constraint_vindicates(salic_prohibition__sovereign_override_reading, pragmatic_sanction_validity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds legislative authority to issue a Pragmatic Sanction overriding the Salic prohibition. The sovereign's override is presented as a legitimate exercise of sovereign legislative power. Challengers to the override are treated as rebels. The sovereign bears the political and military cost of defending the override (e.g., War of Austrian Succession).
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, sovereign, agenda_setter,
    institutional, generational, arbitrage, national).

% Direct beneficiaries of the male-only succession rule. Their claims are secured by the prohibition. They gain the throne and sovereign authority that would otherwise pass to female relatives. Their position depends on the law's enforcement; they resist overrides that threaten their inheritance.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, male_dynastic_heirs, beneficiary,
    powerful, biographical, constrained, national).

% Women of the royal bloodline legally barred from inheriting the crown. Their dynastic rights are extinguished by the Salic prohibition. Some (e.g., Maria Theresa) become sovereigns only when the prohibition is overridden; others (e.g., Joan II of Navarre) are permanently excluded. They have no legal exit within the framework; their only recourse is political alliance or marriage.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, excluded_female_heirs, payer,
    moderate, biographical, trapped, national).

% The landed nobility and representative estates (e.g., French Parlements, Hungarian Diet) benefit from a clear, male-only succession rule that minimizes dynastic disputes and foreign intervention. They often swear oaths to uphold the Salic Law (or its override) and gain institutional stability. Their consent is sometimes sought for Pragmatic Sanctions, giving them leverage.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, nobility_estates, beneficiary,
    organized, generational, constrained, national).

% Princes and foreign houses claiming the throne through female lines (e.g., Spanish Bourbons claiming French throne via Philip V; Bavarian and Saxon electors in War of Austrian Succession). They are structurally excluded by the prohibition. Their claims are delegitimized as rebellious when they contest a sovereign override. They pursue claims through war and diplomacy.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, rival_claimants, excluded,
    powerful, biographical, trapped, national).

% European great powers (France, Britain, Prussia, Russia) who intervene in succession disputes. They analyze the Salic Law and its overrides as strategic instruments. They recognize or reject Pragmatic Sanctions based on balance-of-power calculations. Their diplomatic recognition determines whether an override succeeds.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, foreign_powers, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(salic_prohibition__sovereign_override_reading, male_dynastic_heirs).
narrative_ontology:fixing_cost_class(salic_prohibition__sovereign_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, unambiguous male-line succession rule that prevents dynastic civil wars, excludes foreign claimants via female lines, and stabilizes the domestic political order around a single hereditary principle.
% TRANSFER_FUNCTION: Moves the crown and sovereign authority from potential female heirs (and their marital alliances) to the nearest male agnate, concentrating dynastic power in the male line and directing marriage alliances toward male heirs.
% ABSENT_VOICES: Women of the royal bloodline excluded from succession; their perspectives on dynastic continuity, legitimacy, and governance were never formally consulted. Female heirs like Joan II of Navarre (excluded from France 1328) or the daughters of Charles VI (before the Pragmatic Sanction) had no institutional voice in the law that dispossessed them.
% DISAPPEARANCE_RATIONALE: If the Salic prohibition vanished overnight, female heirs would inherit thrones, marriage alliances would shift to secure female succession, foreign powers would lose a primary pretext for intervention (defending male-line legitimacy), and the European dynastic system would reorganize around cognatic or mixed succession — as eventually occurred in the 19th-20th centuries.
% FOUNDING_PROBLEM: Prevent disputed successions and foreign domination by establishing an unambiguous male-only inheritance rule for the crown, invoked first in 1316 (Philip V over Joan II) and 1328 (Philip VI over Edward III of England) to exclude female-line claims that threatened Capetian continuity.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary French jurists (1316-1328) attest the law was invoked to block English claims via Isabella of France; the Estates General of 1328 ratified male-only succession. Modern historians outside the male-line beneficiary set (e.g., Suzanne Reynolds, 'Fiefs and Vassals'; feminist legal historians like Joan Scott) argue the 'Frankish law' justification was a post-hoc construction to exclude specific rivals, not an ancient immutable custom.
narrative_ontology:disappearance_verdict(salic_prohibition__sovereign_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__sovereign_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__sovereign_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(salic_prohibition__sovereign_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__sovereign_override_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__sovereign_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__sovereign_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(salic_prohibition__sovereign_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the systematic exclusion of women from the throne — a transfer of sovereign power to male agnates. Suppression (0.75) is high because the prohibition is actively enforced by parlements, estates, and military; challengers are attainted as rebels. Theater ratio (0.35) is moderate: the law performs a real coordination function (preventing succession wars), but a growing share of its enforcement (especially post-1713) defends the male-line monopoly rather than preventing disputes. The measurement series uses a shared time grid (1316, 1328, 1589, 1713, 1740, 1748) capturing the law's invocation, solidification, use against foreign claims, sovereign override attempt, war, and settlement.
 *
 * PERSPECTIVAL GAP:
 *   From the sovereign's seat (agenda_setter), the constraint is a tool they control — they can invoke or override it. From male heirs' seat (beneficiary), it is a protective shield. From excluded female heirs' seat (payer), it is an absolute barrier. From rival claimants' seat (excluded), it is a weapon wielded against them. The engine computes this divergence; the authored claim (tangled_rope) reflects the structural reality that the law simultaneously coordinates and extracts.
 *
 * DIRECTIONALITY LOGIC:
 *   The sovereign is the agenda_setter with arbitrage exit (can override via Pragmatic Sanction) — directionality near beneficiary end (d ~ 0.15). Male heirs are beneficiaries with constrained exit (depend on the law) — d ~ 0.3. Excluded female heirs are payers, trapped — d ~ 0.95. Nobility estates are beneficiaries with constrained exit — d ~ 0.25. Rival claimants are excluded, trapped — d ~ 0.9. Foreign powers are observers, analytical — d ~ 0.5. The engine computes per-seat effective extraction from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing disputed successions and foreign claims) was live in 1316-1328 but became contested by 1713: the Pragmatic Sanction acknowledged that the male-only rule now *created* the dispute it was meant to prevent (no male heir). The mandate atrophied — the law's coordination function inverted into a source of war — yet the immutable_mandate_reading persisted as a cover for extraction. This reading (sovereign_override) resolves the mandatrophy by asserting sovereign legislative authority to adapt the rule to the dynastic reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    frankish_origin_ambiguity,
    'Was the Salic Law''s application to royal succession an authentic Frankish tribal custom or a Capetian/Valois legal construction retroactively attributed to the Lex Salica?',
    'Comparative analysis of early Capetian succession practices (987-1316) and the textual history of the Lex Salica manuscripts; palaeographic study of when ''Salic Law'' was first invoked for crown succession.',
    'If a Capetian construction, the immutable_mandate_reading''s natural-law claim collapses; the sovereign_override_reading''s positive-law framing is vindicated. If authentic Frankish custom, the sovereign_override_reading must explain why sovereign authority can override immemorial custom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(frankish_origin_ambiguity, empirical, 'Historical authenticity of Salic Law as succession rule').

omega_variable(
    sovereign_override_source,
    'Does the sovereign''s power to override the Salic prohibition derive from within the Salic Law framework (e.g., a clause permitting amendment) or from a sovereign authority that stands above the law?',
    'Juristic analysis of the Pragmatic Sanction of 1713 and its reception by French Parlements and European powers; comparison with earlier French ''lit de justice'' sessions modifying succession.',
    'If override authority is internal to the law, the constraint is a Scaffold with a built-in sunset mechanism. If external (sovereign stands above), the constraint is a Tangled Rope where the sovereign''s arbitrary will can suspend coordination — a different structural type.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereign_override_source, conceptual, 'Source of sovereign override authority: internal vs. external to the constraint').

omega_variable(
    coordination_vs_extraction_balance,
    'How much of the Salic Law''s persistence (1316-1748) was driven by genuine coordination needs (preventing succession wars) versus male-line extraction (concentrating power in agnates)?',
    'Counterfactual modeling: simulate European dynastic politics under cognatic succession rules; measure frequency of succession disputes under male-only vs. mixed systems in the same period.',
    'If coordination dominates, the constraint trends toward Rope. If extraction dominates, it trends toward Snare. The Tangled Rope classification depends on both being substantial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_balance, empirical, 'Relative weight of coordination function vs. extractive function in the constraint''s persistence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__sovereign_override_reading, 1316, 1748).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(salic_sov_override_tr_t1316, salic_prohibition__sovereign_override_reading, theater_ratio, 1316, 0.1).
narrative_ontology:measurement(salic_sov_override_tr_t1328, salic_prohibition__sovereign_override_reading, theater_ratio, 1328, 0.15).
narrative_ontology:measurement(salic_sov_override_tr_t1589, salic_prohibition__sovereign_override_reading, theater_ratio, 1589, 0.2).
narrative_ontology:measurement(salic_sov_override_tr_t1713, salic_prohibition__sovereign_override_reading, theater_ratio, 1713, 0.3).
narrative_ontology:measurement(salic_sov_override_tr_t1740, salic_prohibition__sovereign_override_reading, theater_ratio, 1740, 0.4).
narrative_ontology:measurement(salic_sov_override_tr_t1748, salic_prohibition__sovereign_override_reading, theater_ratio, 1748, 0.35).

% Extraction over time
narrative_ontology:measurement(salic_sov_override_be_t1316, salic_prohibition__sovereign_override_reading, base_extractiveness, 1316, 0.4).
narrative_ontology:measurement(salic_sov_override_be_t1328, salic_prohibition__sovereign_override_reading, base_extractiveness, 1328, 0.5).
narrative_ontology:measurement(salic_sov_override_be_t1589, salic_prohibition__sovereign_override_reading, base_extractiveness, 1589, 0.55).
narrative_ontology:measurement(salic_sov_override_be_t1713, salic_prohibition__sovereign_override_reading, base_extractiveness, 1713, 0.65).
narrative_ontology:measurement(salic_sov_override_be_t1740, salic_prohibition__sovereign_override_reading, base_extractiveness, 1740, 0.7).
narrative_ontology:measurement(salic_sov_override_be_t1748, salic_prohibition__sovereign_override_reading, base_extractiveness, 1748, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(salic_sov_override_su_t1316, salic_prohibition__sovereign_override_reading, suppression_requirement, 1316, 0.6).
narrative_ontology:measurement(salic_sov_override_su_t1328, salic_prohibition__sovereign_override_reading, suppression_requirement, 1328, 0.7).
narrative_ontology:measurement(salic_sov_override_su_t1589, salic_prohibition__sovereign_override_reading, suppression_requirement, 1589, 0.75).
narrative_ontology:measurement(salic_sov_override_su_t1713, salic_prohibition__sovereign_override_reading, suppression_requirement, 1713, 0.8).
narrative_ontology:measurement(salic_sov_override_su_t1740, salic_prohibition__sovereign_override_reading, suppression_requirement, 1740, 0.85).
narrative_ontology:measurement(salic_sov_override_su_t1748, salic_prohibition__sovereign_override_reading, suppression_requirement, 1748, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__sovereign_override_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(salic_prohibition__sovereign_override_reading, 0.1).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, pragmatic_sanction_1713).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, war_of_austrian_succession).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, salic_prohibition__immutable_mandate_reading).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, salic_prohibition__cognatic_reversion_reading).

% DUAL FORMULATION NOTE:
% This reading decomposes the 'Salic Law' label into a constraint where the prohibition is positive law revocable by sovereign act. The immutable_mandate_reading treats the same prohibition as natural law; the cognatic_reversion_reading treats it as inapplicable custom. All three share the kernel_id 'salic_prohibition' but instantiate different constraints with different ε, beneficiaries, and types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
