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
 *   constraint_id: salic_prohibition__cognatic_reversion_reading
 *   human_readable: Salic Law Prohibition on Female Succession in Non-Frankish Territories (Cognatic Reversion Reading)
 *   domain: constitutional_law/dynastic_succession/political_history
 *
 * SUMMARY:
 *   This constraint story represents the cognatic_reversion_reading of the
 *   salic_prohibition kernel. It treats the prohibition on female succession
 *   in non-Frankish territories as a constructed constraint — a Frankish
 *   tribal custom (Salic Law) that was never intended to bind non-Frankish
 *   lands but was extended through dynastic marriage, conquest, and legal
 *   fiction to serve the interests of male agnatic heirs and dynasties
 *   preserving agnatic succession. The reading asserts that territorial
 *   integrity and the natural right of cognatic primogeniture (eldest child
 *   regardless of sex) should prevail over the anachronistic imposition of
 *   Frankish law. The constraint extracts succession rights from female heirs
 *   and legislative sovereignty from non-Frankish territories, transferring
 *   them to male agnatic relatives and the Frankish legal tradition. Active
 *   enforcement (parlement arrêts, treaties like the Treaty of Utrecht,
 *   military suppression of rival claimants) is required to maintain the
 *   prohibition. The coordination function — preventing dynastic disputes
 *   through a clear rule — is contested: historical evidence suggests Salic
 *   Law often created disputes by excluding plausible female heirs.
 *
 * KEY AGENTS:
 *   - male_agnatic_heirs: Primary beneficiary (institutional/arbitrage) — receive succession rights transferred from female heirs
 *   - dynastic_houses_preserving_agnatic_succession: Agenda setter (institutional/arbitrage) — administer and enforce the prohibition through parlements, treaties, and house laws
 *   - female_heirs_excluded_from_succession: Primary victim (powerless/trapped) — bear the full extraction, excluded from succession with no exit
 *   - non_frankish_territories_subject_to_frankish_law: Victim (organized/constrained) — lose legislative sovereignty over their own succession laws
 *   - competing_cognatic_claimants: Excluded (moderate/trapped) — would press cognatic claims but are structurally barred from the succession conversation
 *   - legal_scholars_jurists: Observer (analytical/analytical) — analyze and debate the legitimacy of Salic Law's territorial extension
 *   - papal_authority: Observer (institutional/analytical) — sometimes legitimizes, sometimes challenges the extension depending on political context
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__cognatic_reversion_reading, 0.75).
domain_priors:suppression_score(salic_prohibition__cognatic_reversion_reading, 0.8).
domain_priors:theater_ratio(salic_prohibition__cognatic_reversion_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__cognatic_reversion_reading, tangled_rope).
narrative_ontology:human_readable(salic_prohibition__cognatic_reversion_reading, "Salic Law Prohibition on Female Succession in Non-Frankish Territories (Cognatic Reversion Reading)").
narrative_ontology:topic_domain(salic_prohibition__cognatic_reversion_reading, "constitutional_law/dynastic_succession/political_history").

domain_priors:requires_active_enforcement(salic_prohibition__cognatic_reversion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__cognatic_reversion_reading, 'cf33968a-d90c-455b-9a2f-c9b3d22f2cb6').
narrative_ontology:cs_kernel_codification('cf33968a-d90c-455b-9a2f-c9b3d22f2cb6', formalized).
narrative_ontology:cs_authority_grounding('cf33968a-d90c-455b-9a2f-c9b3d22f2cb6', lineage).
narrative_ontology:cs_interpretation_layer_present('cf33968a-d90c-455b-9a2f-c9b3d22f2cb6').
narrative_ontology:cs_reading_relation('cf33968a-d90c-455b-9a2f-c9b3d22f2cb6', salic_prohibition__immutable_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('cf33968a-d90c-455b-9a2f-c9b3d22f2cb6', salic_prohibition__sovereign_override_reading, coexists_with).
narrative_ontology:cs_axiom('cf33968a-d90c-455b-9a2f-c9b3d22f2cb6', foundational, salic_law_territorially_limited_to_frankish_origin).
narrative_ontology:cs_axiom_status(salic_law_territorially_limited_to_frankish_origin, holdable).
narrative_ontology:cs_axiom_grounding('cf33968a-d90c-455b-9a2f-c9b3d22f2cb6', salic_law_territorially_limited_to_frankish_origin, empirically_contingent).
narrative_ontology:cs_axiom('cf33968a-d90c-455b-9a2f-c9b3d22f2cb6', foundational, cognatic_primogeniture_as_default_succession).
narrative_ontology:cs_axiom_status(cognatic_primogeniture_as_default_succession, holdable).
narrative_ontology:cs_axiom_grounding('cf33968a-d90c-455b-9a2f-c9b3d22f2cb6', cognatic_primogeniture_as_default_succession, deontological).
narrative_ontology:cs_reference_frame('cf33968a-d90c-455b-9a2f-c9b3d22f2cb6', frankish_tribal_succession_custom).
narrative_ontology:cs_drift_state('cf33968a-d90c-455b-9a2f-c9b3d22f2cb6', early_modern_dynastic_consolidation, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('cf33968a-d90c-455b-9a2f-c9b3d22f2cb6', '2026-06-15T14:30:00Z').
narrative_ontology:cs_kernel_id(salic_prohibition__cognatic_reversion_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, male_agnatic_heirs).
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, dynastic_houses_preserving_agnatic_succession).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, female_heirs_excluded_from_succession).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, non_frankish_territories_subject_to_frankish_law).
narrative_ontology:constraint_vindicates(salic_prohibition__cognatic_reversion_reading, territorial_integrity_over_agnatic_purity).
narrative_ontology:constraint_vindicates(salic_prohibition__cognatic_reversion_reading, cognatic_primogeniture_as_natural_succession).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive succession rights (crowns, territories, revenues) that would otherwise pass to female heirs. Their position is structural: they benefit from the constraint's operation without administering it. They can accept or renounce succession (arbitrage-grade exit), but the constraint's existence creates the option value they capture.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, male_agnatic_heirs, beneficiary,
    institutional, generational, arbitrage, continental).

% Administer and enforce the prohibition through parlements (France), house laws (Habsburg, Bourbon), and international treaties (Utrecht 1713, Vienna 1815). They collect dynastic continuity and alliance stability from the constraint. They could change the rule (as Spain did in 1830, France in 1830 via July Monarchy) but choose not to when it serves their interest.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, dynastic_houses_preserving_agnatic_succession, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__cognatic_reversion_reading, dynastic_houses_preserving_agnatic_succession, beneficiary).

% Bear total exclusion from succession solely by virtue of sex. No exit exists: they cannot change sex, cannot renounce gender, and in most periods cannot effectively press military claims. Their claims are delegitimized by the constraint itself (Salic Law declares them incapable of succeeding). Some (Isabella I of Castile, Maria Theresa, Queen Victoria) succeeded only where the constraint did not apply or was overridden.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, female_heirs_excluded_from_succession, payer,
    powerless, biographical, trapped, continental).

% Lose legislative sovereignty over their own succession laws. Territories like Navarre, Aragon, Naples, Spain (post-1713) had cognatic or mixed succession traditions overridden by Salic Law imposition. Exit requires revolution (Carlist Wars), great-power intervention, or dynastic extinction. Their estates/cortes sometimes resisted (Navarre's fueros, Aragon's cortes) but were overruled by centralizing monarchies invoking Salic Law.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, non_frankish_territories_subject_to_frankish_law, payer,
    organized, generational, constrained, national).

% Would press cognatic succession claims (e.g., descendants of elder daughters excluded by Salic Law) but are structurally barred from the succession conversation. The constraint's enforcement machinery (parlement rulings, treaty recognition) treats their claims as non-justiciable. Some launch armed rebellions (Carlists in Spain, legitimists in France) but these are framed as dynastic disputes, not legal challenges to the constraint.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, competing_cognatic_claimants, excluded,
    moderate, biographical, trapped, continental).

% Analyze and debate Salic Law's original scope, its reception in non-Frankish lands, and the legitimacy of its extension. Figures like Brissaud, Viollet, Ganshof, and modern historians of law provide the empirical basis for this reading's claim that Salic Law was personal law, not territorial law. Their work does not change the constraint's operation but structures the intellectual field.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, legal_scholars_jurists, observer,
    analytical, civilizational, analytical, universal).

% Sometimes legitimizes Salic Law extension (e.g., papal recognition of Bourbon Spain's Salic Law), sometimes challenges it (e.g., papal support for female claimants when politically expedient). The papacy's position tracks its temporal interests, not a consistent legal principle. It occupies an analytical seat because it observes and occasionally intervenes but does not administer the constraint.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, papal_authority, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, unambiguous male-only succession rule that prevents dynastic disputes and fragmentation by eliminating female claimants and their potential husbands/partitions.
% TRANSFER_FUNCTION: Moves succession rights (crowns, territories, revenues) from female heirs and their issue to male agnatic relatives; moves legislative sovereignty over succession from non-Frankish territorial estates to Frankish legal tradition and the dynastic houses that administer it.
% ABSENT_VOICES: Female heirs themselves (structurally silenced by the constraint's gendered exclusion); non-Frankish territorial estates and cortes that were overridden without consent (Navarre's fueros, Aragon's cortes); merchant and urban classes in non-Frankish territories who preferred stable cognatic succession over dynastic wars.
% DISAPPEARANCE_RATIONALE: If the Salic prohibition vanished overnight, female heirs would immediately become eligible successors across Europe; dynastic marriage markets would reconfigure (no need to seek male-only matches for alliance); territories like Navarre and Spain would revert to their pre-Salic cognatic customs; the Carlist Wars and similar legitimacy conflicts would lose their legal basis; the Frankish legal tradition would lose its extraterritorial authority.
% FOUNDING_PROBLEM: Frankish tribal need for clear male military leadership succession in the early medieval period (5th-8th centuries), where the king's primary function was war-leader and the tribe's survival depended on unambiguous male succession.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (Brissaud 'Histoire des institutions françaises', Viollet 'Histoire du droit civil français', Ganshof 'Frankish Institutions') document Salic Law's origin as Frankish tribal custom (Lex Salica) governing a warrior society where the king was first among warriors. No contemporary Frankish source claims universal territorial application; the law applied to Franks as a people (personal law), not to a territory. The extension to non-Frankish lands is universally acknowledged by historians to be a later development (post-1316 in France, post-1713 in Spain) driven by dynastic politics, not Frankish legal theory.
narrative_ontology:disappearance_verdict(salic_prohibition__cognatic_reversion_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__cognatic_reversion_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__cognatic_reversion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(salic_prohibition__cognatic_reversion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__cognatic_reversion_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__cognatic_reversion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__cognatic_reversion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(salic_prohibition__cognatic_reversion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75) because the constraint transfers the crown itself — the supreme political asset — from female heirs to male agnatic relatives, and transfers legislative sovereignty from non-Frankish territories to a foreign legal tradition. Suppression is higher (0.8) because the prohibition's persistence in non-Frankish lands depends entirely on active enforcement: parlement rulings, dynastic house laws, treaties imposed by great powers, and military suppression of rival claimants (e.g., Carlist Wars). Theater ratio is moderate (0.4): the 'ancient tradition' framing is performative cover for a constraint whose actual operation is dynastic interest protection. Accessibility collapse is moderate (0.6): cognatic succession remains legally and practically available (as in England, Castile, Navarre) but is politically suppressed. Resistance is high (0.7): female heirs pressed claims (e.g., Juana of Castile, Maria Theresa), territories resisted (Navarre, Aragon), and jurists debated the law's applicability for centuries.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (dynastic houses), the constraint appears as legitimate tradition preserving dynastic continuity. From the victim seats (female heirs, non-Frankish territories), it appears as foreign imposition and gendered extraction. The engine computes this divergence from the structural data: male_agnatic_heirs have arbitrage-grade exit (they can accept or renounce succession), while female_heirs_excluded_from_succession are trapped (no exit from gender), and non_frankish_territories are constrained (exit requires revolution or great-power intervention).
 *
 * DIRECTIONALITY LOGIC:
 *   Male agnatic heirs and dynastic houses are structural beneficiaries (d near 0.0) — they collect the crown and legislative authority. Female heirs are full targets (d near 1.0) — they bear total extraction with identity-locked exit (cannot change sex, cannot exit gendered exclusion). Non-Frankish territories are high-target (d ~0.8) — they lose legislative sovereignty, with constrained exit (requires international treaty change or revolution). Competing cognatic claimants are excluded (d ~0.9) — their claims are structurally barred. Legal scholars and papal authority are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Frankish tribal need for male military leadership) is dead — the Frankish tribal context vanished by the 9th century. Yet the constraint persists and expands. This is classic mandatrophy: a coordination mechanism for one context (Frankish tribal succession) becomes an extraction mechanism when transplanted to alien contexts (feudal monarchies, early modern states) where its original justification does not apply. The constraint persists because it now benefits powerful dynastic actors who administer it, not because it solves a live coordination problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    salic_original_territorial_scope,
    'Was Salic Law ever understood by its Frankish authors to bind territories beyond the Frankish heartland, or is its extra-Frankish application a later legal fiction?',
    'Comparative analysis of early Frankish legal manuscripts (Pactus Legis Salicae, Lex Salica) versus later glossators'' commentaries; examination of whether Frankish law claimed universal jurisdiction or personal law jurisdiction.',
    'If Salic Law was originally personal law (applying only to Franks), its territorial extension to non-Frankish lands is a constructed imposition, not a natural expansion — supporting this reading''s claim of anachronism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(salic_original_territorial_scope, empirical, 'Original territorial scope of Salic Law versus later territorialized readings').

omega_variable(
    coordination_function_genuineness,
    'Does the male-only succession rule genuinely prevent dynastic disputes, or does it create disputes by excluding plausible female claimants and triggering agnatic-cognatic conflicts?',
    'Historical frequency analysis: compare succession crisis rates in Salic territories versus cognatic territories (e.g., England, Castile, Navarre) controlling for dynasty size and external pressures.',
    'If Salic Law generates more disputes than it prevents, its coordination function is pretextual and the constraint is closer to snare; if it genuinely reduces disputes, the tangled_rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_function_genuineness, empirical, 'Whether the claimed coordination benefit (dispute prevention) is real or cover').

omega_variable(
    extension_mechanisms_legitimacy,
    'By what mechanisms was Salic Law extended to non-Frankish territories (conquest, dynastic marriage, papal decree, legal fiction), and do those mechanisms carry legitimate authority?',
    'Case-by-case genealogy of Salic Law reception in France (post-1316), Spain (Bourbon introduction 1713), Naples, and other territories — identifying the specific legal instrument and its contemporaneous acceptance.',
    'If extension mechanisms are conquest or legal fiction without local consent, the constraint''s legitimacy in those territories collapses; if received by local estates or cortes, the constraint has some coordination legitimacy there.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extension_mechanisms_legitimacy, conceptual, 'Legitimacy of the mechanisms that extended Salic Law beyond Frankish lands').

omega_variable(
    kernel_reading_framing,
    'Is this constraint one reading of the contested kernel ''salic_prohibition'', and how does it relate to the immutable_mandate_reading and sovereign_override_reading?',
    'Structural comparison of the three readings'' beneficiary/victim structures, claimed types, and axiomatic foundations — mapping the kernel''s deformation space.',
    'Confirms this reading instantiates a distinct constraint with its own ε, not a mere interpretation of a single constraint; the engine must treat each reading as a separate constraint story linked by network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Committer-frame structuring: this story is the cognatic_reversion_reading of kernel salic_prohibition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__cognatic_reversion_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(salic_cognatic_tr_t0, salic_prohibition__cognatic_reversion_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(salic_cognatic_tr_t25, salic_prohibition__cognatic_reversion_reading, theater_ratio, 25, 0.15).
narrative_ontology:measurement(salic_cognatic_tr_t50, salic_prohibition__cognatic_reversion_reading, theater_ratio, 50, 0.25).
narrative_ontology:measurement(salic_cognatic_tr_t75, salic_prohibition__cognatic_reversion_reading, theater_ratio, 75, 0.35).
narrative_ontology:measurement(salic_cognatic_tr_t100, salic_prohibition__cognatic_reversion_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(salic_cognatic_be_t0, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(salic_cognatic_be_t25, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 25, 0.35).
narrative_ontology:measurement(salic_cognatic_be_t50, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 50, 0.55).
narrative_ontology:measurement(salic_cognatic_be_t75, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 75, 0.7).
narrative_ontology:measurement(salic_cognatic_be_t100, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 100, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(salic_cognatic_su_t0, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(salic_cognatic_su_t25, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 25, 0.4).
narrative_ontology:measurement(salic_cognatic_su_t50, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 50, 0.6).
narrative_ontology:measurement(salic_cognatic_su_t75, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 75, 0.75).
narrative_ontology:measurement(salic_cognatic_su_t100, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 100, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__cognatic_reversion_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(salic_prohibition__cognatic_reversion_reading, 0.08).
narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, salic_prohibition__immutable_mandate_reading).
narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, salic_prohibition__sovereign_override_reading).
narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, pragmatic_sanction_1713).
narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, carlist_wars_succession_constraint).

% DUAL FORMULATION NOTE:
% This story decomposes the 'Salic Law' label into three structurally distinct constraints per the ε-invariance principle. The immutable_mandate_reading treats the prohibition as mountain (natural/divine law, ε≈0). The sovereign_override_reading treats it as scaffold/rope (positive law, revocable, ε low but non-zero). This reading treats the extra-Frankish application as tangled_rope (coordination cover for extraction, ε=0.75). The ε values differ by a wide margin because the referents differ: immutable_mandate_reading assesses the Frankish tribal custom itself; this reading assesses the extra-Frankish imposition. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(salic_prohibition__cognatic_reversion_reading, institutional, 0.1).
constraint_indexing:directionality_override(salic_prohibition__cognatic_reversion_reading, powerless, 0.95).
constraint_indexing:directionality_override(salic_prohibition__cognatic_reversion_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
