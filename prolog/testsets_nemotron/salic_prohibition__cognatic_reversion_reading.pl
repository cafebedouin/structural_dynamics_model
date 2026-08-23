% ============================================================================
% CONSTRAINT STORY: salic_prohibition__cognatic_reversion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__cognatic_reversion_reading, []).

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
 *   constraint_id: salic_prohibition__cognatic_reversion_reading
 *   human_readable: Salic Law — Cognatic Reversion Reading
 *   domain: constitutional_law/dynastic_succession/political_history
 *
 * SUMMARY:
 *   Salic Law originated as the Lex Salica, a 6th-century Frankish customary
 *   code whose succession provisions (exclusion of women from inheriting
 *   'Salic land') were invoked in 1316 and 1328 to exclude female heirs to
 *   the French crown. Over centuries, this Frankish personal law was elevated
 *   into a 'fundamental law of the French monarchy' and extended by dynastic
 *   accident to territories never subject to Frankish custom — the Spanish
 *   Netherlands, Naples, Milan, Lorraine — through personal unions. The
 *   cognatic reversion reading holds that Salic Law was never properly
 *   binding outside its original Frankish territorial jurisdiction (the
 *   principle of personality of laws: lex terrae and lex personalis), that
 *   its extension was a usurpation by male cadet branches, and that the law's
 *   persistence into the early modern period is theatrical — maintained by
 *   beneficiaries who no longer need its coordination function. The
 *   constraint is claimed as a piton: a once-functional coordination
 *   mechanism (preventing partible inheritance and foreign consort control
 *   among Franks) that atrophied as primogeniture and consort-limitation laws
 *   solved the original problems, but persists through institutional inertia
 *   and the vested interests of male collaterals.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__cognatic_reversion_reading, 0.12).
domain_priors:suppression_score(salic_prohibition__cognatic_reversion_reading, 0.08).
domain_priors:theater_ratio(salic_prohibition__cognatic_reversion_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__cognatic_reversion_reading, piton).
narrative_ontology:human_readable(salic_prohibition__cognatic_reversion_reading, "Salic Law — Cognatic Reversion Reading").
narrative_ontology:topic_domain(salic_prohibition__cognatic_reversion_reading, "constitutional_law/dynastic_succession/political_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__cognatic_reversion_reading, '09d44287-0a52-47d1-8f65-fd8b72eb364c').
narrative_ontology:cs_kernel_codification('09d44287-0a52-47d1-8f65-fd8b72eb364c', fixed_text).
narrative_ontology:cs_authority_grounding('09d44287-0a52-47d1-8f65-fd8b72eb364c', lineage).
narrative_ontology:cs_interpretation_layer_present('09d44287-0a52-47d1-8f65-fd8b72eb364c').
narrative_ontology:cs_reading_relation('09d44287-0a52-47d1-8f65-fd8b72eb364c', salic_prohibition__immutable_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('09d44287-0a52-47d1-8f65-fd8b72eb364c', salic_prohibition__sovereign_override_reading, influences).
narrative_ontology:cs_axiom('09d44287-0a52-47d1-8f65-fd8b72eb364c', foundational, lex_personalitas_frankish_only).
narrative_ontology:cs_axiom_status(lex_personalitas_frankish_only, holdable).
narrative_ontology:cs_axiom_grounding('09d44287-0a52-47d1-8f65-fd8b72eb364c', lex_personalitas_frankish_only, conventional).
narrative_ontology:cs_axiom('09d44287-0a52-47d1-8f65-fd8b72eb364c', foundational, territorial_integrity_supersedes_agnatic_purity).
narrative_ontology:cs_axiom_status(territorial_integrity_supersedes_agnatic_purity, holdable).
narrative_ontology:cs_axiom_grounding('09d44287-0a52-47d1-8f65-fd8b72eb364c', territorial_integrity_supersedes_agnatic_purity, instrumental).
narrative_ontology:cs_reference_frame('09d44287-0a52-47d1-8f65-fd8b72eb364c', frankish_personal_law_framework).
narrative_ontology:cs_drift_state('09d44287-0a52-47d1-8f65-fd8b72eb364c', early_modern_composite_monarchy_era, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('09d44287-0a52-47d1-8f65-fd8b72eb364c', '').
narrative_ontology:cs_kernel_id(salic_prohibition__cognatic_reversion_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, dynastic_cadets_male_line).
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, traditionalist_estates_general_factions).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, cognatic_heirs_female_line).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, non_frankish_territories_under_personal_union).
narrative_ontology:constraint_vindicates(salic_prohibition__cognatic_reversion_reading, territorial_integrity_over_agnatic_purity).
narrative_ontology:constraint_vindicates(salic_prohibition__cognatic_reversion_reading, lex_terrae_territoriality_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the crown subject to Salic Law under the immutable mandate reading; under this reading, the monarch's authority to secure succession includes interpreting territorial applicability. Bound by coronation oaths and estate confirmations that reference the law, but with legislative capacity to clarify its scope.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, reigning_monarch, agenda_setter,
    institutional, biographical, constrained, national).

% Daughters and female-line descendants of the sovereign who would inherit under cognatic primogeniture but are excluded by Salic Law's operation. Their identity and dynastic role are constituted by the exclusion; exit means renouncing dynastic status entirely. Bear the full cost of the constraint's persistence.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, cognatic_heirs_female_line, payer,
    moderate, biographical, identity_locked, national).

% Male-line collateral relatives who stand to inherit if the direct male line fails. They benefit from the exclusion of female heirs without administering the constraint. Their position is portable across territorial boundaries; they can press claims in multiple jurisdictions where the law operates differently.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, dynastic_cadets_male_line, beneficiary,
    powerful, generational, mobile, national).

% Territories acquired by marriage, inheritance, or conquest outside the original Frankish heartland (e.g., Spanish Netherlands, Naples, Lombardy) that never received Salic Law as local custom but are subjected to it through personal union with a Salic crown. They bear the cost of a succession rule that has no roots in their legal tradition and threatens their separate estates and institutions.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, non_frankish_territories_under_personal_union, payer,
    organized, generational, constrained, regional).

% Noble and clerical factions in the Estates General or equivalent bodies that treat Salic Law as a fundamental law of the realm, binding the monarch. They benefit from the stability and predictability it provides for their own privileges and property rights, which are interwoven with the agnatic succession framework.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, traditionalist_estates_general_factions, beneficiary,
    organized, generational, constrained, national).

% Jurists trained in the ius commune who analyze Salic Law as a lex barbarorum — a personal law of the Franks — and argue its territorial inapplicability under the principle of personality of laws. They provide the intellectual framework for the cognatic reversion reading but hold no formal authority to enforce it.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, roman_law_jurists, observer,
    moderate, civilizational, analytical, continental).

% Sovereigns of other states who hold claims through female lines excluded by Salic Law (e.g., Habsburgs, Bourbons). They are structurally excluded from the internal succession discourse but their dynastic interests drive external intervention. They would object to the constraint's application to block their claims but are not seated in the domestic constitutional conversation.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, foreign_powers_with_dynastic_claims, excluded,
    powerful, biographical, arbitrage, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a clear, non-negotiable succession rule that prevented partition disputes among Frankish warrior aristocracy in the early medieval period; coordinated expectations across a personal-law system where law followed the person, not the territory.
% TRANSFER_FUNCTION: Moves the crown and its associated domains from the cognatic heir (eldest child regardless of sex) to the nearest male agnate, transferring sovereignty, patronage networks, and territorial integrity from female lines to male collaterals. In non-Frankish territories, transfers the succession right from local customary law (often cognatic) to an imported Frankish rule.
% ABSENT_VOICES: The non-Frankish territories themselves — their estates, urban elites, and customary legal institutions — were never consulted when Salic Law was extended to them through personal union. Female heirs are present as dynastic persons but silenced as political actors by the very identity_lock that constitutes their exclusion. Roman law jurists who argued for territoriality of laws were heard in counsel but not in legislation.
% DISAPPEARANCE_RATIONALE: If Salic Law vanished overnight, the immediate succession in multiple crowns would revert to cognatic primogeniture or local custom, redrawing the map of Europe: the Habsburg succession would have passed to Maria Theresa without contest, the Spanish and Austrian crowns might not have separated, the War of Spanish Succession and War of Austrian Succession would lack their dynastic casus belli. The territorial integrity of composite monarchies would be renegotiated from the ground up.
% FOUNDING_PROBLEM: Early Frankish kingdoms required a succession mechanism that prevented the kingdom from being divided among all sons (partible inheritance) while also preventing the crown from passing to a woman whose marriage might bring a foreign husband into control of the Frankish host. Salic Law solved this by restricting succession to males of the blood.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is dead: partible inheritance was abandoned centuries later in favor of primogeniture generally; the fear of foreign consort control was addressed by consort-limitation laws (e.g., the English Act of Settlement, the Spanish Pragmatic Sanction) without requiring total female exclusion. No contemporary jurist outside the benefiting cadet branches argues that the original Frankish military rationale applies to 18th-century composite monarchies. The persistence of the law is attested only by its beneficiaries (cadet males, traditionalist estates) as a fundamental law — a self-assertion.
narrative_ontology:disappearance_verdict(salic_prohibition__cognatic_reversion_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__cognatic_reversion_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__cognatic_reversion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(salic_prohibition__cognatic_reversion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__cognatic_reversion_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__cognatic_reversion_reading_tests).
:- end_tests(salic_prohibition__cognatic_reversion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low at the origin (1316: 0.05) because the law genuinely coordinated Frankish succession expectations. It rises as the law is exported to non-Frankish territories (1550: 0.15) where it extracts succession rights from local customs and female heirs. Theater ratio rises dramatically (1316: 0.1 → 1713: 0.75) because the law's coordination function is replaced by performative invocation — the Pragmatic Sanction of 1713 spends enormous diplomatic capital to secure recognition of a succession the law itself forbids, revealing the law as theater. Suppression requirement peaks at 1713 (0.6) when the Habsburgs must enforce the law against both internal claimants and external powers, then collapses to 0.08 by 1789 as the French Revolution renders the constraint moot in its homeland. The constraint is a piton because no single party benefits enough to maintain it actively (the monarchs themselves seek to override it via Pragmatic Sanctions), but the cost of fixing it (renegotiating the fundamental law through Estates General) is prohibitive relative to the benefit for any single actor.
 *
 * PERSPECTIVAL GAP:
 *   From the monarch's seat (agenda_setter), the constraint appears as a binding fundamental law they must either enforce or laboriously override — a piton they inherit. From the cognatic heir's seat (payer), it is a snare: pure extraction of their birthright, maintained by identity_lock. From the non-Frankish territory's seat (payer), it is a tangled rope: the personal union provides coordination (shared defense, trade, dynasty) but the succession rule extracts their autonomy. From the cadet male's seat (beneficiary), it is a rope: a clear coordination rule that benefits them. The engine computes these divergences from the structural data; the claimed piton type reflects the system-level view where the constraint's original function has atrophied but no actor can unilaterally retire it.
 *
 * DIRECTIONALITY LOGIC:
 *   The reigning monarch is the agenda setter but structurally constrained — d ≈ 0.5 (symmetric) because the monarch both administers the law and is bound by it, yet seeks to override it for dynastic survival. Cognatic heirs (female line) are full targets — d ≈ 0.95 — identity_locked by dynastic role, excluded from the succession they would otherwise inherit. Non-Frankish territories are targets — d ≈ 0.85 — organized but constrained exit (secession risks war, loss of imperial protection). Male cadets are beneficiaries — d ≈ 0.15 — mobile across jurisdictions, collecting the extraction without administering it. Traditionalist estates are beneficiaries — d ≈ 0.2 — constrained exit (their privileges depend on the agnatic framework). Roman law jurists are analytical observers — d = 0.0. Foreign powers are excluded — d ≈ 0.9 but with arbitrage exit (they can press claims through war or marriage diplomacy).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing partible inheritance and foreign consort control among Frankish warriors) is dead — solved by universal primogeniture and consort-limitation statutes across Europe. The constraint persists as a piton because: (1) the monarchs who could fix it (via legislative override) face prohibitive fixing costs — convening Estates General risks broader constitutional demands; (2) the beneficiaries (cadet males, traditionalist estates) are diffuse and each too weak to maintain it alone but collectively block reform; (3) the payers (cognatic heirs, non-Frankish territories) are identity-locked or constrained, unable to coordinate a coalition. The mandatrophy is resolved in the sense that the law's original mandate is acknowledged as obsolete even by its defenders, who now defend it as 'fundamental law' — a tautological justification. The constraint is a zombie: dead mandate, living enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    territorial_vs_personal_law_ambiguity,
    'Was Salic Law understood in the 14th–18th centuries as a territorial law (lex terrae) binding all within the kingdom, or a personal law (lex personalis) binding only Franks and their descendants?',
    'Comparative analysis of contemporary juristic commentaries (e.g., Du Tillet, Le Caron, Bodin) and the actual practice of parlements registering edicts — did they treat the law as attached to the crown (territorial) or to the Frankish blood (personal)?',
    'If personal law, the cognatic reversion reading is structurally correct: the law never legitimately extended to non-Frankish territories. If territorial law, the immutable mandate reading gains coherence. The classification shifts from piton (anachronism) toward tangled_rope (coordination with extraction) or snare (if the territorial extension was knowingly extractive).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_vs_personal_law_ambiguity, conceptual, 'Whether Salic Law''s jurisdictional basis was territorial or personal — the core structural ambiguity between readings.').

omega_variable(
    pragmatic_sanction_as_override_or_confirmation,
    'Does the Pragmatic Sanction of 1713 (Charles VI) and 1723 (Philip V) represent a sovereign override of Salic Law (supporting sovereign_override_reading) or a confirmation of its fundamental status by seeking external guarantors for an exception?',
    'Diplomatic history of the guarantor negotiations: did the emperor seek recognition of his sovereign right to alter succession, or recognition of a dispensation from a higher law?',
    'If override, the sovereign_override_reading is validated and Salic Law is positive law (lower theater, higher monarch agency). If dispensation, the immutable_mandate_reading is reinforced and the cognatic reversion reading''s claim that the law was never binding on non-Frankish lands is strengthened (the emperor needed guarantors precisely because he couldn''t simply legislate).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pragmatic_sanction_as_override_or_confirmation, empirical, 'Whether the Pragmatic Sanctions treated Salic Law as overridable positive law or as a higher law requiring dispensation.').

omega_variable(
    mandatrophy_timing,
    'At what point did the coordination function of Salic Law (preventing partition and foreign consort control) become fully obsolete, and was the law''s persistence after that point maintained by any actor''s active intent or purely by inertia?',
    'Correlate the adoption of primogeniture and consort-limitation laws across European monarchies with the rhetorical shift in Salic Law defenses — from ''necessary for stability'' to ''fundamental law of the realm''.',
    'If a clear obsolescence date exists and persistence was purely inertial, the piton classification is confirmed. If active maintenance by beneficiaries continued (e.g., cadets lobbying parlements), the constraint leans toward snare. If the monarchs actively maintained it for their own stability, it leans toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_timing, empirical, 'When the original coordination mandate died and whether persistence was inertial or actively maintained.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__cognatic_reversion_reading, 1316, 1789).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(salic_prohibition__cognatic_reversion_reading_tr_t1316, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1316, 0.1).
narrative_ontology:measurement(salic_prohibition__cognatic_reversion_reading_tr_t1350, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1350, 0.2).
narrative_ontology:measurement(salic_prohibition__cognatic_reversion_reading_tr_t1450, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1450, 0.35).
narrative_ontology:measurement(salic_prohibition__cognatic_reversion_reading_tr_t1550, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1550, 0.5).
narrative_ontology:measurement(salic_prohibition__cognatic_reversion_reading_tr_t1650, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1650, 0.6).
narrative_ontology:measurement(salic_prohibition__cognatic_reversion_reading_tr_t1713, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1713, 0.75).
narrative_ontology:measurement(salic_prohibition__cognatic_reversion_reading_tr_t1789, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1789, 0.65).

% Extraction over time
narrative_ontology:measurement(salic_prohibition__cognatic_reversion_reading_be_t1316, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1316, 0.05).
narrative_ontology:measurement(salic_prohibition__cognatic_reversion_reading_be_t1350, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1350, 0.08).
narrative_ontology:measurement(salic_prohibition__cognatic_reversion_reading_be_t1450, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1450, 0.1).
narrative_ontology:measurement(salic_prohibition__cognatic_reversion_reading_be_t1550, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1550, 0.15).
narrative_ontology:measurement(salic_prohibition__cognatic_reversion_reading_be_t1650, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1650, 0.18).
narrative_ontology:measurement(salic_prohibition__cognatic_reversion_reading_be_t1713, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1713, 0.22).
narrative_ontology:measurement(salic_prohibition__cognatic_reversion_reading_be_t1789, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1789, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(salic_prohibition__cognatic_reversion_reading_su_t1316, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1316, 0.1).
narrative_ontology:measurement(salic_prohibition__cognatic_reversion_reading_su_t1350, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1350, 0.15).
narrative_ontology:measurement(salic_prohibition__cognatic_reversion_reading_su_t1450, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1450, 0.2).
narrative_ontology:measurement(salic_prohibition__cognatic_reversion_reading_su_t1550, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1550, 0.3).
narrative_ontology:measurement(salic_prohibition__cognatic_reversion_reading_su_t1650, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1650, 0.45).
narrative_ontology:measurement(salic_prohibition__cognatic_reversion_reading_su_t1713, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1713, 0.6).
narrative_ontology:measurement(salic_prohibition__cognatic_reversion_reading_su_t1789, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1789, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__cognatic_reversion_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(salic_prohibition__cognatic_reversion_reading, 0.1).
narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, pragmatic_sanction_1713).
narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, pragmatic_sanction_1723).
narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, war_of_spanish_succession).
narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, war_of_austrian_succession).
narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, salic_prohibition__immutable_mandate_reading).
narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, salic_prohibition__sovereign_override_reading).

% DUAL FORMULATION NOTE:
% The salic_prohibition kernel decomposes into three readings: this cognatic_reversion_reading (territorial inapplicability, piton), immutable_mandate_reading (fundamental law, mountain/tangled_rope), and sovereign_override_reading (positive law, scaffold/rope). They share the same historical text but differ on authority_grounding, spatial_scope, and the status of the founding problem. Linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(salic_prohibition__cognatic_reversion_reading, institutional, 0.5).
constraint_indexing:directionality_override(salic_prohibition__cognatic_reversion_reading, moderate, 0.95).
constraint_indexing:directionality_override(salic_prohibition__cognatic_reversion_reading, organized, 0.85).
constraint_indexing:directionality_override(salic_prohibition__cognatic_reversion_reading, powerful, 0.15).
constraint_indexing:directionality_override(salic_prohibition__cognatic_reversion_reading, organized, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
