% ============================================================================
% CONSTRAINT STORY: imperial_mandate__bakufu_delegation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imperial_mandate__bakufu_delegation_reading, []).

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
 *   constraint_id: imperial_mandate__bakufu_delegation_reading
 *   human_readable: Bakufu Delegation of Divine Mandate
 *   domain: political_philosophy/east_asian_history
 *
 * SUMMARY:
 *   The bakufu_delegation_reading instantiates a specific reading of the
 *   imperial_mandate kernel: divine mandate operates through institutional
 *   delegation, bifurcating sovereignty into a ritual emperor (legitimacy
 *   source) and a military shogun (governance executor). This constraint
 *   governed Japan from the Kamakura founding (1192) through the Tokugawa
 *   bakufu (1603-1868). It presents as coordination — solving succession,
 *   pacification, and administration — but extracts heavily from peasants and
 *   outer lords while suppressing direct imperial rule and alternative
 *   legitimacy claims. The claimed_type is tangled_rope: genuine coordination
 *   function (pacification, stable administration) coexists with asymmetric
 *   extraction (peasant surplus → samurai stipends) and active enforcement
 *   (sankin-kotai, censorship, seclusion laws).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_mandate__bakufu_delegation_reading, 0.42).
domain_priors:suppression_score(imperial_mandate__bakufu_delegation_reading, 0.55).
domain_priors:theater_ratio(imperial_mandate__bakufu_delegation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__bakufu_delegation_reading, tangled_rope).
narrative_ontology:human_readable(imperial_mandate__bakufu_delegation_reading, "Bakufu Delegation of Divine Mandate").
narrative_ontology:topic_domain(imperial_mandate__bakufu_delegation_reading, "political_philosophy/east_asian_history").

domain_priors:requires_active_enforcement(imperial_mandate__bakufu_delegation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__bakufu_delegation_reading, '2c7224f8-ebd6-4400-b994-d23345dced36').
narrative_ontology:cs_kernel_codification('2c7224f8-ebd6-4400-b994-d23345dced36', distributed).
narrative_ontology:cs_authority_grounding('2c7224f8-ebd6-4400-b994-d23345dced36', lineage).
narrative_ontology:cs_interpretation_layer_present('2c7224f8-ebd6-4400-b994-d23345dced36').
narrative_ontology:cs_reading_relation('2c7224f8-ebd6-4400-b994-d23345dced36', imperial_mandate__loyalist_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('2c7224f8-ebd6-4400-b994-d23345dced36', foundational, legitimacy_separable_from_exercise).
narrative_ontology:cs_axiom_status(legitimacy_separable_from_exercise, holdable).
narrative_ontology:cs_axiom_grounding('2c7224f8-ebd6-4400-b994-d23345dced36', legitimacy_separable_from_exercise, conventional).
narrative_ontology:cs_axiom('2c7224f8-ebd6-4400-b994-d23345dced36', foundational, samurai_class_as_legitimate_governing_stratum).
narrative_ontology:cs_axiom_status(samurai_class_as_legitimate_governing_stratum, overridden).
narrative_ontology:cs_axiom_grounding('2c7224f8-ebd6-4400-b994-d23345dced36', samurai_class_as_legitimate_governing_stratum, conventional).
narrative_ontology:cs_reference_frame('2c7224f8-ebd6-4400-b994-d23345dced36', kamakura_delegation_settlement).
narrative_ontology:cs_drift_state('2c7224f8-ebd6-4400-b994-d23345dced36', bakumatsu_crisis, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('2c7224f8-ebd6-4400-b994-d23345dced36', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(imperial_mandate__bakufu_delegation_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, samurai_class).
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, bakufu_administration).
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, imperial_court_ritual).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, peasant_producers).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, outer_lords_tozama).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, imperial_authority_direct).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, emperor_ritual).
narrative_ontology:constraint_vindicates(imperial_mandate__bakufu_delegation_reading, legitimacy_delegation_doctrine).
narrative_ontology:constraint_vindicates(imperial_mandate__bakufu_delegation_reading, dual_sovereignty_model).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reigns as sacred symbol and source of legitimacy; receives court stipend and ritual honors; politically confined to palace by bakufu protocol; cannot refuse investiture of shogun without triggering crisis; identity fused to ritual role such that political action would destroy the legitimacy the bakufu needs
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, emperor_ritual, beneficiary,
    institutional, generational, identity_locked, national).

% Exercises actual governance — taxation, justice, foreign relations, domain oversight; derives authority from imperial commission but controls the terms of that commission; maintains emperor in seclusion to secure its own position; could modify the arrangement but bears cost of legitimacy loss if it breaks delegation form
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, bakufu_administration, agenda_setter,
    institutional, generational, arbitrage, national).

% Monopoly on legitimate violence and administrative office; stipended by bakufu domains; identity bound to service ethos and status hierarchy; exit means becoming ronin — status collapse and economic precarity; benefits from arrangement but bears its military burdens
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, samurai_class, beneficiary,
    organized, biographical, constrained, national).

% Bear the tax burden (40-60% of harvest) supporting the entire edifice; no political voice; bound to land by village collective responsibility system; flight punished; the arrangement extracts their surplus to fund samurai stipends and bakufu administration
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, peasant_producers, payer,
    powerless, immediate, trapped, local).

% Domains not integrated into bakufu hereditary vassalage; retain autonomy but owe ceremonial submission and sankin-kotai costs; monitored by fudai neighbors; could rebel but face coalition of bakufu + fudai; pay tribute in form and substance without full participation in governance
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, outer_lords_tozama, payer,
    powerful, biographical, constrained, regional).

% Maintains court ceremonies, calendar, and culture; receives bakufu funding for rituals; legitimizes shoguns through investiture; identity fused to ritual continuity; cannot challenge bakufu without losing the material base that sustains the court
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, imperial_court_ritual, beneficiary,
    moderate, generational, identity_locked, local).

% Argue that mandate requires direct imperial rule; develop kokugaku and sonno thought; suppressed by bakufu censorship and persecution; would object to delegation as usurpation but excluded from power; their ideas become revolutionary fuel in 1860s
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, loyalist_intellectuals, excluded,
    moderate, biographical, constrained, national).

% Encounter Japan as a unified sovereign state under shogun's seal; negotiate treaties with bakufu; later discover emperor as alternative legitimacy source; exploit the bifurcation to extract concessions; their presence accelerates the constraint's terminal crisis
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, foreign_powers, observer,
    institutional, biographical, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the succession and legitimacy problem in a polity where sacred sovereignty cannot be exercised directly without risking its sacrality: the emperor's ritual purity is preserved by delegating profane governance to a military administrator who bears the stigma of coercion, while the samurai class provides a stable ruling stratum that converts violence into ordered administration.
% TRANSFER_FUNCTION: Moves agricultural surplus from peasant producers upward through domain lords to bakufu and samurai stipends; moves legitimate authority downward from emperor through commission to shogun to daimyo to local administrators; moves status and identity from birth/merit into the frozen four-class hierarchy (shi-no-ko-sho).
% ABSENT_VOICES: Peasant producers have no representative voice; outer lords (tozama) are structurally excluded from central councils; loyalist intellectuals are censored and persecuted; merchant class (chonin) grows economically powerful but remains status-subordinate with no political channel.
% DISAPPEARANCE_RATIONALE: If the delegation constraint vanished overnight, the emperor would face the choice of resuming direct rule (which the court lacks capacity for) or the polity would fragment into competing warlord domains; the samurai class would lose its legitimating warrant; the tax system would collapse without bakufu coordination; the entire Tokugawa order would reorganize around a new sovereignty claim.
% FOUNDING_PROBLEM: After centuries of imperial decline and warlord fragmentation (Sengoku), the bakufu delegation solved: (1) how to restore central authority without exposing the sacred throne to the pollution of governance, and (2) how to bind a military class into stable administration rather than predatory competition.
% FOUNDING_PROBLEM_CORROBORATION: Tokugawa Ieyasu's own testament (legacy of Ieyasu) and bakufu institutional records attest the delegation was a pragmatic settlement for pacification, not a permanent constitutional form; Meiji oligarchs (Ito Hirobumi, Yamagata Aritomo) explicitly treated the founding problem as resolved by the Restoration itself; modern Japanese constitutional scholarship (e.g., Maruyama Masao) confirms the delegation was a historical expedient whose rationale expired.
narrative_ontology:disappearance_verdict(imperial_mandate__bakufu_delegation_reading, world_rearranges).
narrative_ontology:founding_problem_status(imperial_mandate__bakufu_delegation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__bakufu_delegation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(imperial_mandate__bakufu_delegation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imperial_mandate__bakufu_delegation_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imperial_mandate__bakufu_delegation_reading_tests).
:- end_tests(imperial_mandate__bakufu_delegation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42→0.48) reflects the tax burden on peasants and the opportunity cost to outer lords; suppression (0.55→0.72) rises as bakufu enforces seclusion and censors loyalist thought; theater (0.15→0.42) increases as ritual performance expands while administrative vitality stagnates. The shared time grid (8 points) shows parallel drift: extraction and theater rise together, suppression accelerates in the terminal phase. Accessibility_collapse (0.45) is moderate — alternatives (direct imperial rule, republicanism, domain independence) exist but are structurally blocked. Resistance (0.35) is low for most of the interval, spiking only in the 1850s-60s.
 *
 * PERSPECTIVAL GAP:
 *   From bakufu_administration's seat, the constraint is a rope: it built the system, maintains order, and could reform it. From peasant_producers' seat, it is a snare: extraction without voice, exit blocked. From emperor_ritual's seat, it is a mountain: the delegation appears as the natural form of sovereignty. The engine computes these divergences from the structural data — the claimed tangled_rope captures the aggregate tension.
 *
 * DIRECTIONALITY LOGIC:
 *   Emperor_ritual: identity_locked beneficiary — receives legitimacy and material support but cannot exit the role without destroying the system's legitimacy logic (d ~0.1). Bakufu_administration: agenda_setter with arbitrage exit — controls the constraint but could modify it at legitimacy cost (d ~0.3). Samurai_class: organized beneficiary with constrained exit — benefits from status/stipend but bound by service identity (d ~0.25). Peasant_producers: powerless trapped payers — no exit, full extraction (d ~0.95). Outer_lords_tozama: powerful constrained payers — autonomy within submission, exit means war (d ~0.6). Imperial_court_ritual: moderate identity_locked beneficiary — ritual funding dependent on bakufu (d ~0.15). Loyalist_intellectuals: moderate constrained excluded — ideas suppressed but not eradicated (d ~0.7). Foreign_powers: institutional arbitrage observers — exploit bifurcation (d ~0.05).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (pacification after Sengoku, protecting sacred sovereignty from governance pollution) was solved by ~1650. The arrangement persisted 200+ years after its rationale expired — classic mandatrophy. The bakufu could not dismantle itself because its authority derived from the very delegation it would have to revoke. The samurai class became a rentier stratum. The theater_ratio rise (0.15→0.42) tracks the conversion of administrative function into ritual performance. The loyalist_restoration_reading emerged as the counter-reading that named the mandatrophy and supplied the revolutionary vocabulary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the imperial_mandate kernel, and how does it structurally differ from the loyalist_restoration_reading?',
    'Compare the beneficiary/victim structures: bakufu_delegation_reading makes samurai_class and bakufu_administration beneficiaries with emperor_ritual as identity_locked beneficiary; loyalist_restoration_reading makes emperor_direct the agenda_setter with samurai_class as payer. The disagreement is located on whether legitimacy requires mediation (delegation) or immediacy (direct exercise).',
    'If the readings are distinct constraints with different ε values (this reading: moderate extraction with coordination; loyalist: low extraction but high suppression of alternatives), they must be authored separately and linked via network.affects_constraints. Conflating them would violate ε-invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural distinction between bakufu delegation and loyalist restoration as readings of the imperial mandate kernel.').

omega_variable(
    legitimacy_coordination_vs_extraction,
    'Does the delegation arrangement genuinely coordinate a stable polity (rope function) or primarily extract surplus for the samurai-bakufu bloc (snare function)?',
    'Measure the ratio of administrative services delivered (dispute resolution, infrastructure, famine relief, currency stability) to surplus extracted from peasants. Compare Tokugawa outcomes to Sengoku baseline and Meiji successor.',
    'If coordination dominates, the claimed tangled_rope is validated; if extraction dominates, the constraint reclassifies toward snare. The engine''s per-seat computation will reveal whether samurai_class experiences net benefit or net cost over the interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_coordination_vs_extraction, empirical, 'Whether the delegation''s coordination function is genuine or a cover for samurai-bakufu extraction.').

omega_variable(
    emperor_identity_lock_mechanism,
    'Is the emperor''s identity_locked exit status structural (bakufu enforcement of seclusion) or internalized (court internalization of ritual role as self-concept)?',
    'Track court behavior at moments of bakufu weakness (e.g., 1850s-60s): did the court attempt political reassertion when enforcement slackened, or did it continue ritual performance? The Meiji Restoration''s use of the emperor suggests latent agency.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the emperor carries the constraint''s logic even after bakufu enforcement fails. This affects directionality derivation for the emperor_ritual seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emperor_identity_lock_mechanism, empirical, 'Structural vs. internalized identity lock for the ritual emperor.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__bakufu_delegation_reading, 1192, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(imperial_mandate_bakufu_tr_t1192, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1192, 0.15).
narrative_ontology:measurement(imperial_mandate_bakufu_tr_t1336, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1336, 0.18).
narrative_ontology:measurement(imperial_mandate_bakufu_tr_t1467, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1467, 0.22).
narrative_ontology:measurement(imperial_mandate_bakufu_tr_t1603, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1603, 0.25).
narrative_ontology:measurement(imperial_mandate_bakufu_tr_t1700, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1700, 0.28).
narrative_ontology:measurement(imperial_mandate_bakufu_tr_t1800, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1800, 0.32).
narrative_ontology:measurement(imperial_mandate_bakufu_tr_t1853, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1853, 0.38).
narrative_ontology:measurement(imperial_mandate_bakufu_tr_t1868, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1868, 0.42).

% Extraction over time
narrative_ontology:measurement(imperial_mandate_bakufu_be_t1192, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1192, 0.35).
narrative_ontology:measurement(imperial_mandate_bakufu_be_t1336, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1336, 0.38).
narrative_ontology:measurement(imperial_mandate_bakufu_be_t1467, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1467, 0.4).
narrative_ontology:measurement(imperial_mandate_bakufu_be_t1603, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1603, 0.41).
narrative_ontology:measurement(imperial_mandate_bakufu_be_t1700, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1700, 0.42).
narrative_ontology:measurement(imperial_mandate_bakufu_be_t1800, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1800, 0.43).
narrative_ontology:measurement(imperial_mandate_bakufu_be_t1853, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1853, 0.45).
narrative_ontology:measurement(imperial_mandate_bakufu_be_t1868, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1868, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(imperial_mandate_bakufu_su_t1192, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1192, 0.45).
narrative_ontology:measurement(imperial_mandate_bakufu_su_t1336, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1336, 0.48).
narrative_ontology:measurement(imperial_mandate_bakufu_su_t1467, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1467, 0.52).
narrative_ontology:measurement(imperial_mandate_bakufu_su_t1603, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1603, 0.55).
narrative_ontology:measurement(imperial_mandate_bakufu_su_t1700, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1700, 0.56).
narrative_ontology:measurement(imperial_mandate_bakufu_su_t1800, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1800, 0.58).
narrative_ontology:measurement(imperial_mandate_bakufu_su_t1853, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1853, 0.65).
narrative_ontology:measurement(imperial_mandate_bakufu_su_t1868, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1868, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_mandate__bakufu_delegation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(imperial_mandate__bakufu_delegation_reading, 0.12).
narrative_ontology:affects_constraint(imperial_mandate__bakufu_delegation_reading, imperial_mandate__loyalist_restoration_reading).
narrative_ontology:affects_constraint(imperial_mandate__bakufu_delegation_reading, tokugawa_sankin_kotai).
narrative_ontology:affects_constraint(imperial_mandate__bakufu_delegation_reading, tokugawa_class_hierarchy).
narrative_ontology:affects_constraint(imperial_mandate__bakufu_delegation_reading, meiji_restoration_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint and loyalist_restoration_reading form the imperial_mandate kernel family. This reading (bakufu_delegation) treats legitimacy as delegable; the sibling (loyalist_restoration) treats it as inseparable from exercise. Their ε values differ: this reading shows moderate extraction with coordination (ε≈0.42); the sibling shows low extraction but high suppression of the delegation arrangement (ε≈0.25 for its own referent). They are linked bidirectionally via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imperial_mandate__bakufu_delegation_reading, institutional, 0.15).
constraint_indexing:directionality_override(imperial_mandate__bakufu_delegation_reading, powerful, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
