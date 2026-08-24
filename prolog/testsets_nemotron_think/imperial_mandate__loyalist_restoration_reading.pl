% ============================================================================
% CONSTRAINT STORY: imperial_mandate__loyalist_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imperial_mandate__loyalist_restoration_reading, []).

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
 *   constraint_id: imperial_mandate__loyalist_restoration_reading
 *   human_readable: Loyalist Restoration Reading: Unmediated Imperial Sovereignty
 *   domain: political/historical/east_asian
 *
 * SUMMARY:
 *   The loyalist restoration reading (sonnō jōi → fukko ishin) asserts that
 *   the Japanese emperor's divine mandate (kokutai) requires direct exercise
 *   of sovereignty — not merely ritual legitimation of a delegated
 *   administrator (shogun). The bakufu's acceptance of unequal treaties
 *   without imperial sanction is framed as usurpation. The constraint demands
 *   institutional rupture: overthrow of Tokugawa bakufu, abolition of feudal
 *   domains, and restoration of emperor as active governing sovereign. This
 *   reading powered the Meiji Restoration (1868). The claimed_type is
 *   tangled_rope: genuine coordination function (unified command for
 *   modernization) coexists with asymmetric extraction (bakufu and samurai
 *   stripped of status/power, commoners conscripted into new order).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_mandate__loyalist_restoration_reading, 0.82).
domain_priors:suppression_score(imperial_mandate__loyalist_restoration_reading, 0.78).
domain_priors:theater_ratio(imperial_mandate__loyalist_restoration_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__loyalist_restoration_reading, tangled_rope).
narrative_ontology:human_readable(imperial_mandate__loyalist_restoration_reading, "Loyalist Restoration Reading: Unmediated Imperial Sovereignty").
narrative_ontology:topic_domain(imperial_mandate__loyalist_restoration_reading, "political/historical/east_asian").

domain_priors:requires_active_enforcement(imperial_mandate__loyalist_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__loyalist_restoration_reading, 'ea33579c-886c-4584-bff2-62da7e828075').
narrative_ontology:cs_kernel_codification('ea33579c-886c-4584-bff2-62da7e828075', fixed_text).
narrative_ontology:cs_authority_grounding('ea33579c-886c-4584-bff2-62da7e828075', lineage).
narrative_ontology:cs_interpretation_layer_present('ea33579c-886c-4584-bff2-62da7e828075').
narrative_ontology:cs_reading_relation('ea33579c-886c-4584-bff2-62da7e828075', imperial_mandate__bakufu_delegation_reading, forecloses).
narrative_ontology:cs_axiom('ea33579c-886c-4584-bff2-62da7e828075', foundational, emperor_as_sole_sovereign).
narrative_ontology:cs_axiom_status(emperor_as_sole_sovereign, holdable).
narrative_ontology:cs_axiom_grounding('ea33579c-886c-4584-bff2-62da7e828075', emperor_as_sole_sovereign, theological).
narrative_ontology:cs_axiom('ea33579c-886c-4584-bff2-62da7e828075', foundational, direct_rule_necessary_for_legitimacy).
narrative_ontology:cs_axiom_status(direct_rule_necessary_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('ea33579c-886c-4584-bff2-62da7e828075', direct_rule_necessary_for_legitimacy, deontological).
narrative_ontology:cs_axiom('ea33579c-886c-4584-bff2-62da7e828075', secondary, bakufu_as_usurpation).
narrative_ontology:cs_axiom_status(bakufu_as_usurpation, holdable).
narrative_ontology:cs_axiom_grounding('ea33579c-886c-4584-bff2-62da7e828075', bakufu_as_usurpation, theological).
narrative_ontology:cs_reference_frame('ea33579c-886c-4584-bff2-62da7e828075', kokutai_national_polity).
narrative_ontology:cs_drift_state('ea33579c-886c-4584-bff2-62da7e828075', bakufu_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('ea33579c-886c-4584-bff2-62da7e828075', '').
narrative_ontology:cs_kernel_id(imperial_mandate__loyalist_restoration_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, emperor).
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, court_nobles).
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, loyalist_daimyo).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, bakufu).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, samurai_class).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, loyalist_daimyo).
narrative_ontology:constraint_vindicates(imperial_mandate__loyalist_restoration_reading, kokutai_national_polity).
narrative_ontology:constraint_vindicates(imperial_mandate__loyalist_restoration_reading, imperial_lineage_unbroken).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Embodiment of divine mandate; legitimacy inseparable from active governance. No exit from role — identity fused with sovereignty claim. Restoration requires personal initiative in modernization and foreign engagement.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, emperor, agenda_setter,
    institutional, generational, identity_locked, national).

% Regain political influence and administrative posts lost to bakufu. Their status derives from proximity to restored imperial center. Exit means accepting bakufu patronage or irrelevance.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, court_nobles, beneficiary,
    organized, biographical, constrained, national).

% Domains like Satsuma, Chōshū, Tosa, Hizen. Gain political legitimacy and central authority by championing restoration. Bear military and financial costs of overthrowing bakufu. Exit means submission to bakufu or destruction.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, loyalist_daimyo, beneficiary,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__loyalist_restoration_reading, loyalist_daimyo, payer).

% Tokugawa shogunate holding delegated administrative authority. Delegitimized as usurpers by loyalist reading. Must either suppress restoration movement or surrender authority. No institutional exit — dissolution is the constraint's objective.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, bakufu, payer,
    institutional, biographical, trapped, national).

% Warrior class whose status and stipends depend on bakufu-domain system. Restoration abolishes feudal domains and stipends, replacing with conscript army and merit bureaucracy. Identity fused to warrior role; exit means status collapse.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, samurai_class, payer,
    organized, biographical, identity_locked, national).

% Peasants, merchants, artisans. Bear taxation and conscription costs of both bakufu and restoration wars. No voice in sovereignty contest; loyalty demanded by whichever authority prevails. Exit geographically or socially nearly impossible.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, commoners, excluded,
    powerless, immediate, trapped, local).

% Western treaty powers (US, UK, France, Russia, Netherlands). Observe sovereignty contest to assess treaty counterparty stability. Prefer unified negotiating partner but exploit bakufu-loyalist division. Not subject to constraint but shape its enforcement context.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, foreign_powers, observer,
    powerful, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves competing legitimacy claims by asserting single sovereign authority: emperor as both ritual and administrative head, eliminating dual structure (emperor/bakufu) that creates policy paralysis and foreign vulnerability.
% TRANSFER_FUNCTION: Moves governing authority, revenue control, military command, and foreign treaty power from bakufu and domain administrations to imperial center. Transfers status and stipends from samurai class to new merit-based bureaucracy.
% ABSENT_VOICES: Commoners who bear war costs but have no representation; peripheral domains (e.g., Aizu, Ōuetsu Reppan Dōmei) that resist restoration and are crushed; bakufu loyalists who see delegation as legitimate constitutional order; merchants who benefited from bakufu commercial stability.
% DISAPPEARANCE_RATIONALE: If loyalist constraint vanished, bakufu delegation would persist — no Meiji Restoration, no centralized modernization, likely continued unequal treaties and eventual colonization. The constraint's enforcement (Boshin War, abolition of domains) restructured Japanese polity entirely.
% FOUNDING_PROBLEM: Bakufu's inability to respond coherently to foreign pressure (Perry 1853, unequal treaties) while maintaining domestic order; dual sovereignty creates decision paralysis where emperor has legitimacy but no power, bakufu has power but declining legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: Foreign diplomatic records (Townsend Harris, Ernest Satow) document bakufu decision paralysis; bakufu's own reform attempts (Bunkyū Reforms) acknowledge crisis; Meiji oligarchs' memoirs (Itō Hirobumi, Yamagata Aritomo) attest founding problem was live but also that restoration created new extraction structures.
narrative_ontology:disappearance_verdict(imperial_mandate__loyalist_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(imperial_mandate__loyalist_restoration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__loyalist_restoration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imperial_mandate__loyalist_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imperial_mandate__loyalist_restoration_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imperial_mandate__loyalist_restoration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imperial_mandate__loyalist_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imperial_mandate__loyalist_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   High extractiveness (0.82) reflects bakufu/samurai perspective: the constraint strips their institutional existence. But from loyalist view, this is recovery of usurped sovereignty — extraction is contested. Suppression (0.78) is high: bakufu actively suppresses loyalists (Ansei Purge), then loyalists suppress bakufu (Boshin War). Theater_ratio (0.42) moderate: imperial rituals (Daijō-kan, Shinto rites) perform ancient continuity while new institutions (conscription, taxation, prefectures) are built. Accessibility_collapse (0.88) high: bakufu delegation framed as illegitimate, no middle ground. Resistance (0.71) high: bakufu forces, northern domains, samurai rebellions (Satsuma 1877).
 *
 * PERSPECTIVAL GAP:
 *   From emperor/loyalist seat: constraint is coordination (restoring natural order). From bakufu/samurai seat: constraint is extraction (usurpation of their legitimate authority). From commoner seat: constraint is opaque restructuring — new extraction replaces old. Engine computes per-seat χ from these structural positions. The bakufu_delegation_reading would compute opposite directionalities.
 *
 * DIRECTIONALITY LOGIC:
 *   Emperor is identity_locked agenda_setter — cannot exit the mandate without ceasing to be emperor. Court nobles and loyalist daimyo are beneficiaries with constrained exit (their power depends on restoration success). Bakufu is trapped payer — institutional dissolution is the constraint's telos. Samurai are identity_locked payers — warrior identity fused to bakufu-domain system. Commoners are trapped excluded — no voice, bear costs. Foreign powers are analytical observers shaping context. Directionality derives from beneficiary/victim declarations + identity_locked exit for emperor/samurai.
 *
 * MANDATROPHY ANALYSIS:
 *   Loyalist claim: mandate is eternal (Mountain). Structural analysis: mandate operates as Tangled Rope — coordination against foreign threat + extraction from bakufu/samurai. Mandatrophy risk: after restoration, the 'direct rule' mandate becomes Piton — emperor reigns but oligarchs govern; mandate becomes theatrical legitimation for new extractive structures (peerage, conscription, state Shinto). Founding problem (foreign threat) shifts from live to dead, but constraint persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_natural_vs_constructed,
    'Is the emperor''s direct sovereignty a genuine natural/divine law (Mountain) or a constructed ideology deployed by court nobles and peripheral daimyo to seize power from bakufu?',
    'Comparative analysis of pre-Meiji political thought: whether sonnō ideology predates bakufu crisis or emerges as reaction to it; archaeological/philological study of ancient ritsuryō texts vs. Tokugawa-era kokugaku reconstructions.',
    'If natural law, constraint is Mountain with negligible extraction. If constructed, extraction is real and classification shifts to Tangled Rope/Snare. False Summit Mountain detection would trigger if Mountain claimed with beneficiaries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandate_natural_vs_constructed, conceptual, 'Natural-law vs. constructed-ideology ambiguity at kernel core.').

omega_variable(
    suppression_mechanism_loyalist,
    'Is the high suppression measured (bakufu→loyalist then loyalist→bakufu) structural (military/police power) or internalized (belief in mandate''s absoluteness making compromise unthinkable)?',
    'Post-restoration trajectory: if suppression persists after bakufu destroyed (e.g., against samurai rebellions, popular rights movement), internalized component confirmed. Track thought-policing (Peace Preservation Laws) as continuation.',
    'If internalized, effective suppression higher than structural measure — mandate becomes self-enforcing cognitive constraint. Explains Piton persistence after founding problem dies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_loyalist, empirical, 'Structural vs. internalized suppression in mandate enforcement.').

omega_variable(
    cs_framing_underdetermination,
    'Does the kernel''s authority ground in the imperial lineage itself (lineage) or in the Shinto ritual system that maintains the lineage''s sacrality (practice/expertise of priesthood)?',
    'Historical analysis of Meiji State Shinto construction: whether priesthood (Jingikan) had independent interpretive authority or was subordinated to oligarchs. Compare pre-1868 court rituals vs. post-1868 state Shinto.',
    'If lineage: authority_grounding=lineage, interpretation_layer_present=true (court rituals). If practice/expertise: authority_grounding=practice/expertise, different drift dynamics. Changes cs_structure classification and foreclosure logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Alternative CS framings of the same kernel produce different authority_grounding values.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__loyalist_restoration_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(imperial_mandate_loyalist_tr_t0, imperial_mandate__loyalist_restoration_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(imperial_mandate_loyalist_tr_t3, imperial_mandate__loyalist_restoration_reading, theater_ratio, 3, 0.38).
narrative_ontology:measurement(imperial_mandate_loyalist_tr_t6, imperial_mandate__loyalist_restoration_reading, theater_ratio, 6, 0.4).
narrative_ontology:measurement(imperial_mandate_loyalist_tr_t9, imperial_mandate__loyalist_restoration_reading, theater_ratio, 9, 0.41).
narrative_ontology:measurement(imperial_mandate_loyalist_tr_t12, imperial_mandate__loyalist_restoration_reading, theater_ratio, 12, 0.42).
narrative_ontology:measurement(imperial_mandate_loyalist_tr_t15, imperial_mandate__loyalist_restoration_reading, theater_ratio, 15, 0.42).

% Extraction over time
narrative_ontology:measurement(imperial_mandate_loyalist_be_t0, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(imperial_mandate_loyalist_be_t3, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 3, 0.72).
narrative_ontology:measurement(imperial_mandate_loyalist_be_t6, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 6, 0.78).
narrative_ontology:measurement(imperial_mandate_loyalist_be_t9, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 9, 0.81).
narrative_ontology:measurement(imperial_mandate_loyalist_be_t12, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 12, 0.83).
narrative_ontology:measurement(imperial_mandate_loyalist_be_t15, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 15, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(imperial_mandate_loyalist_su_t0, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(imperial_mandate_loyalist_su_t3, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 3, 0.62).
narrative_ontology:measurement(imperial_mandate_loyalist_su_t6, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 6, 0.7).
narrative_ontology:measurement(imperial_mandate_loyalist_su_t9, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 9, 0.75).
narrative_ontology:measurement(imperial_mandate_loyalist_su_t12, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 12, 0.78).
narrative_ontology:measurement(imperial_mandate_loyalist_su_t15, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 15, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_mandate__loyalist_restoration_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(imperial_mandate__loyalist_restoration_reading, 0.08).
narrative_ontology:affects_constraint(imperial_mandate__loyalist_restoration_reading, imperial_mandate__bakufu_delegation_reading).
narrative_ontology:affects_constraint(imperial_mandate__loyalist_restoration_reading, meiji_constitution_promulgation).
narrative_ontology:affects_constraint(imperial_mandate__loyalist_restoration_reading, state_shinto_institutionalization).
narrative_ontology:affects_constraint(imperial_mandate__loyalist_restoration_reading, peerage_act_1884).
narrative_ontology:affects_constraint(imperial_mandate__loyalist_restoration_reading, conscription_law_1873).

% DUAL FORMULATION NOTE:
% Imperial mandate kernel decomposes into loyalist_restoration_reading (this constraint) and bakufu_delegation_reading. Loyalist reading has higher extractiveness (0.82 vs ~0.35 estimated for bakufu reading from bakufu perspective) because it demands institutional rupture. Both share coordination_type=identity_coordination but differ in authority_grounding (lineage vs. practice) and reading_relations (forecloses vs. coexists_with).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imperial_mandate__loyalist_restoration_reading, institutional, 0.15).
constraint_indexing:directionality_override(imperial_mandate__loyalist_restoration_reading, powerless, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
