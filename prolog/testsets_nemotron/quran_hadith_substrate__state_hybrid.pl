% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__state_hybrid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_hadith_substrate__state_hybrid, []).

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
 *   constraint_id: quran_hadith_substrate__state_hybrid
 *   human_readable: State Hybrid Adoption of Sharia Substrate
 *   domain: religious/legal/political
 *
 * SUMMARY:
 *   Post-colonial Muslim states constructed a hybrid legal order: classical
 *   fiqh codified in family law and criminal codes (hudud, qisas, blasphemy)
 *   to signal Islamic legitimacy, while commercial, administrative, and
 *   constitutional law followed secular or reformist models for economic
 *   functionality. The state — not the scholarly tradition — decides which
 *   rulings are 'Islamic' and which are 'modern.' Legitimacy is grounded in
 *   political sovereignty (the state's right to define the sharia) rather
 *   than doctrinal fidelity to any madhhab. This reading of the Quran-Hadith
 *   substrate instrumentalizes the text: it is a legitimacy resource the
 *   state manages, not a binding constraint on state action. The constraint
 *   is the *arrangement itself* — the state's selective adoption and
 *   enforcement — not the substrate texts.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__state_hybrid, 0.35).
domain_priors:suppression_score(quran_hadith_substrate__state_hybrid, 0.45).
domain_priors:theater_ratio(quran_hadith_substrate__state_hybrid, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, extractiveness, 0.35).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__state_hybrid, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__state_hybrid, "State Hybrid Adoption of Sharia Substrate").
narrative_ontology:topic_domain(quran_hadith_substrate__state_hybrid, "religious/legal/political").

domain_priors:requires_active_enforcement(quran_hadith_substrate__state_hybrid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__state_hybrid, 'e704af5f-ef48-4144-a7dd-69a294c78130').
narrative_ontology:cs_kernel_codification('e704af5f-ef48-4144-a7dd-69a294c78130', fixed_text).
narrative_ontology:cs_authority_grounding('e704af5f-ef48-4144-a7dd-69a294c78130', extraction).
narrative_ontology:cs_interpretation_layer_present('e704af5f-ef48-4144-a7dd-69a294c78130').
narrative_ontology:cs_reading_relation('e704af5f-ef48-4144-a7dd-69a294c78130', quran_hadith_substrate__traditionalist_taqlid, coexists_with).
narrative_ontology:cs_reading_relation('e704af5f-ef48-4144-a7dd-69a294c78130', quran_hadith_substrate__reformist_ijtihad, influences).
narrative_ontology:cs_axiom('e704af5f-ef48-4144-a7dd-69a294c78130', foundational, state_sovereignty_defines_sharia_scope).
narrative_ontology:cs_axiom_status(state_sovereignty_defines_sharia_scope, holdable).
narrative_ontology:cs_axiom_grounding('e704af5f-ef48-4144-a7dd-69a294c78130', state_sovereignty_defines_sharia_scope, conventional).
narrative_ontology:cs_axiom('e704af5f-ef48-4144-a7dd-69a294c78130', foundational, identity_domains_require_islamic_branding).
narrative_ontology:cs_axiom_status(identity_domains_require_islamic_branding, holdable).
narrative_ontology:cs_axiom_grounding('e704af5f-ef48-4144-a7dd-69a294c78130', identity_domains_require_islamic_branding, instrumental).
narrative_ontology:cs_reference_frame('e704af5f-ef48-4144-a7dd-69a294c78130', classical_fiqh_comprehensive_application).
narrative_ontology:cs_drift_state('e704af5f-ef48-4144-a7dd-69a294c78130', post_colonial_state_formation, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('e704af5f-ef48-4144-a7dd-69a294c78130', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__state_hybrid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, state_elites).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, official_ulama).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, security_apparatus).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, traditionalist_scholars).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, reformist_activists).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, women_under_codified_fiqh).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, minority_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control legislative and executive machinery; selectively codify classical fiqh in family/criminal law to signal Islamic legitimacy while maintaining secular commercial/administrative codes for economic integration. Extract legitimacy rents from Islamic branding without doctrinal constraint on policy autonomy.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, state_elites, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__state_hybrid, state_elites, beneficiary).

% State-appointed religious scholars who authenticate the hybrid framework. Receive institutional salaries, media platforms, and monopoly on fatwa issuance. Their authority depends on not challenging the state's selective adoption; dissent risks removal and replacement.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, official_ulama, beneficiary,
    organized, biographical, constrained, national).

% Enforces the boundary between permitted and prohibited interpretations. Uses blasphemy, apostasy, and public order laws to suppress both traditionalist comprehensive-sharia movements and reformist critical readings. Gains operational latitude and budget from being guardian of 'true Islam' as the state defines it.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, security_apparatus, beneficiary,
    institutional, biographical, mobile, national).

% Advocate comprehensive application of classical fiqh across all domains. Their madhhab-based authority is truncated when the state picks rulings a la carte. Cannot exit the identity frame of 'defending the sharia' without self-negation; pushed to margins, monitored, occasionally co-opted for specific fatwas.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, traditionalist_scholars, payer,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__state_hybrid, traditionalist_scholars, excluded).

% Push for ijtihad aligned with human rights, gender equality, minority protections. Their readings threaten the state's legitimacy formula (which depends on 'Islamic' branding) and the official ulama's monopoly. Face censorship, travel bans, imprisonment; exit means exile or silence.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, reformist_activists, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__state_hybrid, reformist_activists, excluded).

% Subject to codified family law (marriage, divorce, custody, inheritance) that freezes classical patriarchal rulings while the state refuses parallel reforms in economic rights. No meaningful exit: religious identity binds them to the system, secular courts lack jurisdiction, emigration is materially blocked.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, women_under_codified_fiqh, payer,
    powerless, biographical, trapped, local).

% Non-Muslim or minority Muslim communities governed by state's 'Islamic' criminal provisions (blasphemy, hudud) while denied autonomy in personal status. The hybrid system instrumentalizes their subordination for majority legitimation. Exit is geographic displacement or conversion — both costly.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, minority_communities, payer,
    powerless, generational, constrained, regional).

% Condition lending on commercial/administrative legal reforms compatible with global standards. Their pressure reinforces the state's secular-economic track while ignoring the Islamic-family-law track. See the hybridity as pragmatic; do not challenge the legitimacy formula.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, international_financial_institutions, observer,
    institutional, biographical, analytical, global).

% Produce critical scholarship on the hybrid system from academic safety. Their work circulates back into domestic discourse via digital channels, creating a feedback loop the state tries to manage. Not directly coerced but structurally excluded from policy influence.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, diaspora_intellectuals, observer,
    moderate, generational, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legitimacy interface between the state and Muslim-majority populations: the state signals Islamic identity through selective sharia adoption, reducing regime-change pressure from traditionalist constituencies while preserving economic policy space for development and integration.
% TRANSFER_FUNCTION: Moves political legitimacy and social control from the population to state elites via the symbolic capital of 'Islamic law.' The state collects obedience and reduced dissent; traditionalists and reformists pay with suppressed agency. Women and minorities pay with codified subordinate status. Official ulama collect institutional privilege in exchange for authentication.
% ABSENT_VOICES: Classical fuqaha (jurists) of the madhhab tradition who would demand comprehensive application; feminist tafsir scholars who read the Quran as ethical trajectory; victims of hudud punishments who cannot speak; migrant workers subject to the system without citizenship. They are excluded by coercion (traditionalists, reformists), structural position (women, minorities), or geography (diaspora, migrants).
% DISAPPEARANCE_RATIONALE: If the hybrid framework vanished overnight, the state would lose its primary legitimacy idiom, triggering either a traditionalist mobilization demanding full sharia or a secularist rupture. Family law would become a contested vacuum. Commercial law would likely remain secular (path dependence). The official ulama institution would collapse. Security apparatus would need new justification.
% FOUNDING_PROBLEM: Post-colonial Muslim states needed to legitimate themselves to populations with Islamic identity while building modern administrative/economic systems incompatible with comprehensive classical fiqh. The hybrid solution — sharia in 'identity' domains (family, crime), secular in 'functional' domains (commerce, administration) — resolved the immediate legitimation crisis.
% FOUNDING_PROBLEM_CORROBORATION: State elites and official ulama attest the founding problem remains live (populations still demand Islamic identity signaling). Traditionalist scholars attest it is dead (the hybrid is a betrayal, not a solution). Reformists attest it was never the real problem (the problem was authoritarianism using Islam as cover). Independent historians (e.g., Anderson, Hallaq, Messick) document the colonial/post-colonial construction of 'Islamic law' as a state instrument — corroboration outside the beneficiary set.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__state_hybrid, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__state_hybrid, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__state_hybrid, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(quran_hadith_substrate__state_hybrid, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__state_hybrid, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_hadith_substrate__state_hybrid_tests).
:- end_tests(quran_hadith_substrate__state_hybrid_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35) is moderate: the state extracts legitimacy rents and social control from the Islamic branding, but the coordination function (reducing traditionalist opposition) is genuine and non-trivial. Suppression (0.45) is variable across regimes and periods — peaks during Islamist mobilization (1979-1990s) and post-Arab Spring crackdowns (2013+), lower during developmentalist phases. Theater ratio (0.38) reflects that the 'Islamic' performance (official fatwas, sharia councils, hudud statutes) is partly functional (legitimation) and partly performative (masking secular economic policy). Accessibility collapse (0.42) is partial: alternative readings exist (traditionalist, reformist) but are structurally excluded from state power. Resistance (0.55) is significant from both traditionalist and reformist camps, but fragmented and repressed.
 *
 * PERSPECTIVAL GAP:
 *   From the state-elite seat, the arrangement looks like a Rope: it coordinates legitimacy and development, alternatives (full sharia or full secularism) are costlier. From the traditionalist seat, it looks like a Snare: the Islamic label is cover for extracting obedience while violating the sharia's integrity. From the reformist seat, it looks like a Tangled Rope: there is coordination (state capacity) but extraction is asymmetric (women, minorities pay). From the women/minority seat, it is a Snare: no exit, pure extraction. The engine computes these per-seat divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   State elites are primary beneficiaries (d ~ 0.1): they collect legitimacy and policy autonomy. Official ulama are secondary beneficiaries (d ~ 0.2): institutional privilege contingent on compliance. Security apparatus benefits instrumentally (d ~ 0.15). Traditionalist scholars are victims (d ~ 0.85): their comprehensive vision is truncated, identity-locked exit. Reformist activists are victims (d ~ 0.8): critical readings suppressed, constrained exit. Women under codified fiqh are payers (d ~ 0.9): trapped by identity and law. Minority communities are payers (d ~ 0.75): constrained exit, structural subordination. International institutions and diaspora are observers (d ~ 0.5): analytical distance.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-colonial legitimation + modernization) was real but the hybrid solution has outlived its conditions. The arrangement persists because it serves current elites, not because the original problem remains in its original form. Mandatrophy is unresolved: the mandate ('Islamic legitimacy') has become a resource for extraction rather than a solution to a live coordination problem. The theater ratio rise (0.2→0.38) tracks this: more performance, less function. The claim 'tangled_rope' captures the dual reality — genuine coordination function (legitimation interface) plus asymmetric extraction (women, minorities, dissenters pay).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commitment_system_framing_ambiguity,
    'Is the hybrid system best modeled as a commitment system with the state as authoritative interpreter of a fixed kernel (Quran/Hadith), or as a non-commitment-system extraction mechanism that merely mimics CS discourse?',
    'Compare the state''s actual adjudication practice: does it reason *from* the kernel text via interpretive methods (even if selectively), or does it simply legislate outcomes and retrofit Islamic vocabulary? Track fatwa issuance patterns, judicial reasoning, and legislative history across regimes.',
    'If CS framing holds, the constraint has an interpretation layer absorbing drift (cs_structure.interpretation_layer_present=true) and axioms grounded in lineage/extraction. If non-CS, the ''Islamic'' discourse is pure theater — theater_ratio would be higher, and the constraint would classify more cleanly as snare or piton. The cs_structure fields in this story assume the CS framing; this omega documents the alternative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commitment_system_framing_ambiguity, conceptual, 'Whether the state''s Islamic discourse constitutes a genuine commitment system or a mimetic extraction cover.').

omega_variable(
    coordination_extraction_boundary_variability,
    'How much of the measured extractiveness variance (0.25-0.45 across contexts) reflects genuine coordination-function strength vs. regime-specific extraction intensity?',
    'Cross-national comparison of hybrid regimes: measure family-law codification completeness, commercial-law secularization depth, and repression levels against the same time-series metrics. Identify whether high-extractiveness cases share a coordination deficit or an extraction surplus.',
    'If variance is mostly coordination-function variance, the ''tangled_rope'' claim holds across contexts with different ε. If variance is mostly extraction-intensity variance, the constraint family may need decomposition per regime (per ε-invariance principle).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary_variability, empirical, 'Whether cross-context ε variability reflects coordination strength or extraction intensity.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'For women_under_codified_fiqh and minority_communities, is suppression primarily structural (legal barriers, state coercion) or partially internalized (religious identity fused with subordinate status, making exit cognitively unavailable)?',
    'Post-exit trajectory analysis: where women/minorities gain legal exit (emigration, secular court access, reform), does suppression persist as self-censorship, community enforcement, or identity conflict? Compare diaspora vs. in-situ populations.',
    'If internalized, effective suppression is higher than structural measure — the constraint travels with the agent. This would increase χ for identity-locked seats and support stronger piton/snare classification for those seats. The current suppression metric (0.45) captures only structural suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs. internalized suppression for identity-locked victim groups.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__state_hybrid, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qhs_hybrid_tr_t1950, quran_hadith_substrate__state_hybrid, theater_ratio, 1950, 0.2).
narrative_ontology:measurement(qhs_hybrid_tr_t1970, quran_hadith_substrate__state_hybrid, theater_ratio, 1970, 0.25).
narrative_ontology:measurement(qhs_hybrid_tr_t1990, quran_hadith_substrate__state_hybrid, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(qhs_hybrid_tr_t2005, quran_hadith_substrate__state_hybrid, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(qhs_hybrid_tr_t2015, quran_hadith_substrate__state_hybrid, theater_ratio, 2015, 0.42).
narrative_ontology:measurement(qhs_hybrid_tr_t2025, quran_hadith_substrate__state_hybrid, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(qhs_hybrid_be_t1950, quran_hadith_substrate__state_hybrid, base_extractiveness, 1950, 0.25).
narrative_ontology:measurement(qhs_hybrid_be_t1970, quran_hadith_substrate__state_hybrid, base_extractiveness, 1970, 0.3).
narrative_ontology:measurement(qhs_hybrid_be_t1990, quran_hadith_substrate__state_hybrid, base_extractiveness, 1990, 0.38).
narrative_ontology:measurement(qhs_hybrid_be_t2005, quran_hadith_substrate__state_hybrid, base_extractiveness, 2005, 0.33).
narrative_ontology:measurement(qhs_hybrid_be_t2015, quran_hadith_substrate__state_hybrid, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement(qhs_hybrid_be_t2025, quran_hadith_substrate__state_hybrid, base_extractiveness, 2025, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(qhs_hybrid_su_t1950, quran_hadith_substrate__state_hybrid, suppression_requirement, 1950, 0.35).
narrative_ontology:measurement(qhs_hybrid_su_t1970, quran_hadith_substrate__state_hybrid, suppression_requirement, 1970, 0.45).
narrative_ontology:measurement(qhs_hybrid_su_t1990, quran_hadith_substrate__state_hybrid, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(qhs_hybrid_su_t2005, quran_hadith_substrate__state_hybrid, suppression_requirement, 2005, 0.4).
narrative_ontology:measurement(qhs_hybrid_su_t2015, quran_hadith_substrate__state_hybrid, suppression_requirement, 2015, 0.5).
narrative_ontology:measurement(qhs_hybrid_su_t2025, quran_hadith_substrate__state_hybrid, suppression_requirement, 2025, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__state_hybrid, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_hadith_substrate__state_hybrid, 0.1).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, quran_hadith_substrate__traditionalist_taqlid).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, quran_hadith_substrate__reformist_ijtihad).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, state_family_law_codification).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, commercial_law_secularization).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, blasphemy_apostasy_enforcement).

% DUAL FORMULATION NOTE:
% This constraint is one member of the quran_hadith_substrate kernel family. The traditionalist_taqlid reading (authority=lineage, coordination=identity_coordination) and reformist_ijtihad reading (authority=expertise/practice, coordination=identity_coordination) are sibling constraints. This state_hybrid reading (authority=extraction, coordination=identity_coordination) differs in authority_grounding and beneficiary/victim structure. All three share the identity_coordination type because all coordinate Muslim identity boundaries against shifting criteria — but the state_hybrid reading's coordination is state-directed and extractive.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_hadith_substrate__state_hybrid, institutional, 0.1).
constraint_indexing:directionality_override(quran_hadith_substrate__state_hybrid, organized, 0.85).
constraint_indexing:directionality_override(quran_hadith_substrate__state_hybrid, moderate, 0.8).
constraint_indexing:directionality_override(quran_hadith_substrate__state_hybrid, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
