% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__temporal_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__temporal_accommodation_reading, []).

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
 *   constraint_id: eternal_marriage_covenant__temporal_accommodation_reading
 *   human_readable: Temporal Accommodation of Eternal Marriage Covenant
 *   domain: religious_law/political_theology/commitment_system_dynamics
 *
 * SUMMARY:
 *   The 1890 Manifesto (Official Declaration 1) suspended the practice of
 *   plural marriage without renouncing the underlying doctrine (D&C 132) that
 *   defines eternal marriage as a covenant requiring polygamy for exaltation.
 *   This reading — the temporal accommodation — holds that the eternal
 *   principle remains valid but obedience to civil law takes precedence 'for
 *   the present.' The suspension was intended as temporary pending political
 *   conditions that would allow restoration. Over 134 years, the temporary
 *   accommodation has become the permanent operating regime: the church
 *   enforces monogamy excommunicating polygamists while teaching that
 *   polygamy remains an eternal principle. This creates a structural double
 *   bind for members: they must live monogamy while believing polygamy is
 *   celestial law.
 *
 * KEY AGENTS:
 *   - church_institutional_leadership: agenda_setter (institutional/identity_locked) — issues and interprets the Manifesto, manages the doctrinal double bind
 *   - federal_authorities: beneficiary (institutional/arbitrage) — applied coercion that produced the accommodation
 *   - mainstream_mormon_laity: beneficiary/payer (organized/constrained) — gained statehood/normalization, bear cognitive dissonance
 *   - fundamentalist_polygamist_families: payer (powerless/trapped) — continue practice, face persecution/exclusion
 *   - women_in_coerced_plural_marriages: payer (powerless/trapped) — bear the gendered cost of both historical and fundamentalist polygamy
 *   - dissident_mormon_scholars: excluded (moderate/constrained) — challenge coherence, face institutional sanctions
 *   - external_observer_analytical: observer (analytical/analytical) — sees the commitment-system pattern
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__temporal_accommodation_reading, 0.45).
domain_priors:suppression_score(eternal_marriage_covenant__temporal_accommodation_reading, 0.72).
domain_priors:theater_ratio(eternal_marriage_covenant__temporal_accommodation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__temporal_accommodation_reading, scaffold).
narrative_ontology:human_readable(eternal_marriage_covenant__temporal_accommodation_reading, "Temporal Accommodation of Eternal Marriage Covenant").
narrative_ontology:topic_domain(eternal_marriage_covenant__temporal_accommodation_reading, "religious_law/political_theology/commitment_system_dynamics").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__temporal_accommodation_reading).
narrative_ontology:has_sunset_clause(eternal_marriage_covenant__temporal_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__temporal_accommodation_reading, 'ee86c1d4-991b-4bba-b94c-4b6ce5819f3a').
narrative_ontology:cs_kernel_codification('ee86c1d4-991b-4bba-b94c-4b6ce5819f3a', fixed_text).
narrative_ontology:cs_authority_grounding('ee86c1d4-991b-4bba-b94c-4b6ce5819f3a', lineage).
narrative_ontology:cs_interpretation_layer_present('ee86c1d4-991b-4bba-b94c-4b6ce5819f3a').
narrative_ontology:cs_reading_relation('ee86c1d4-991b-4bba-b94c-4b6ce5819f3a', eternal_marriage_covenant__immutable_commandment_reading, coexists_with).
narrative_ontology:cs_reading_relation('ee86c1d4-991b-4bba-b94c-4b6ce5819f3a', eternal_marriage_covenant__prophetic_override_reading, influences).
narrative_ontology:cs_axiom('ee86c1d4-991b-4bba-b94c-4b6ce5819f3a', foundational, manifesto_suspends_practice_not_doctrine).
narrative_ontology:cs_axiom_status(manifesto_suspends_practice_not_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('ee86c1d4-991b-4bba-b94c-4b6ce5819f3a', manifesto_suspends_practice_not_doctrine, conventional).
narrative_ontology:cs_axiom('ee86c1d4-991b-4bba-b94c-4b6ce5819f3a', foundational, eternal_principle_awaits_future_restoration).
narrative_ontology:cs_axiom_status(eternal_principle_awaits_future_restoration, holdable).
narrative_ontology:cs_axiom_grounding('ee86c1d4-991b-4bba-b94c-4b6ce5819f3a', eternal_principle_awaits_future_restoration, deontological).
narrative_ontology:cs_axiom('ee86c1d4-991b-4bba-b94c-4b6ce5819f3a', secondary, obedience_to_civil_law_supersedes_divine_command_temporarily).
narrative_ontology:cs_axiom_status(obedience_to_civil_law_supersedes_divine_command_temporarily, holdable).
narrative_ontology:cs_axiom_grounding('ee86c1d4-991b-4bba-b94c-4b6ce5819f3a', obedience_to_civil_law_supersedes_divine_command_temporarily, instrumental).
narrative_ontology:cs_reference_frame('ee86c1d4-991b-4bba-b94c-4b6ce5819f3a', dormant_eternal_covenant_awaiting_restoration).
narrative_ontology:cs_drift_state('ee86c1d4-991b-4bba-b94c-4b6ce5819f3a', contemporary_institutional_regime, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('ee86c1d4-991b-4bba-b94c-4b6ce5819f3a', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, church_institutional_leadership).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, federal_authorities).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, mainstream_mormon_laity).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, fundamentalist_polygamist_families).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, women_in_coerced_plural_marriages).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, dissident_mormon_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, mainstream_mormon_laity).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__temporal_accommodation_reading, continuing_revelation_doctrine).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__temporal_accommodation_reading, obedience_to_civil_law_principle).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__temporal_accommodation_reading, institutional_survival_as_divine_mandate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issued the 1890 Manifesto suspending polygamous practice while maintaining doctrinal eternal validity. Manages the tension between federal compliance pressure and member expectations. Controls temple rites, excommunication authority, and doctrinal interpretation. Identity is fused to the institution's survival and claimed prophetic mantle.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, church_institutional_leadership, agenda_setter,
    institutional, generational, identity_locked, global).

% Applied military, legal, and economic pressure (Edmunds Act, Edmunds-Tucker Act, disfranchisement, property seizure) to force Mormon abandonment of polygamy. Achieved nominal compliance via the Manifesto while the doctrine remained intact. Could escalate or de-escalate enforcement at will.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, federal_authorities, beneficiary,
    institutional, biographical, arbitrage, national).

% Gained Utah statehood, end of persecution, and social normalization by accepting the Manifesto. Bear psychological cost of cognitive dissonance: taught eternal doctrine is suspended not renounced, creating perpetual tension. Exit means leaving community, family, and salvation narrative.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, mainstream_mormon_laity, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__temporal_accommodation_reading, mainstream_mormon_laity, payer).

% Continue polygamous practice as eternal commandment, facing prosecution, excommunication, social ostracization, and loss of temple access. Their communities are geographically isolated, economically marginalized, and internally coercive — especially for women and children. No recognized exit within their frame.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, fundamentalist_polygamist_families, payer,
    powerless, generational, trapped, local).

% Subject to plural marriage under both pre-Manifesto church practice and post-Manifesto fundamentalist continuation. Experience structural coercion: religious duty, economic dependence, reproductive pressure, and community enforcement. The Manifesto's suspension did not liberate them; fundamentalist splinters intensified their subjection.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, women_in_coerced_plural_marriages, payer,
    powerless, biographical, trapped, local).

% Challenge the doctrinal coherence of 'suspended but eternal' and the institutional claim to prophetic authority. Face excommunication, professional marginalization within Mormon studies, and social shunning. Their critique is structurally excluded from official discourse but persists in academic and post-Mormon spaces.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, dissident_mormon_scholars, excluded,
    moderate, biographical, constrained, global).

% Analyzes the constraint as a commitment-system adaptation under coercion: a kernel (eternal marriage covenant) read through a temporary accommodation that becomes structurally permanent. Sees the pattern of 'doctrine preserved, practice surrendered' as a recurrent form in religious-political negotiation.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, external_observer_analytical, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates institutional survival of the LDS Church under existential federal threat by surrendering the practice that triggered state violence while preserving the doctrinal kernel that structures Mormon soteriology and communal identity.
% TRANSFER_FUNCTION: Transfers the cost of federal non-compliance (property seizure, disfranchisement, imprisonment, institutional death) from the church institution onto fundamentalist dissenters and women in coerced marriages, who bear continued persecution and subjection. Transfers legitimacy to the federal state by performing obedience.
% ABSENT_VOICES: Women in coerced plural marriages (both historical and fundamentalist) — their bodies and lives are the terrain on which the accommodation is enacted, but they hold no seat in the Manifesto's issuance or interpretation. Fundamentalist communities are excluded from the institutional definition of Mormonism despite claiming the original kernel.
% DISAPPEARANCE_RATIONALE: If the temporal accommodation vanished — i.e., if the church either fully renounced the eternal doctrine or fully restored the practice — the Mormon religious field would fracture. Renunciation would collapse the temple-centered soteriology; restoration would reignite federal conflict. The accommodation is the structural hinge holding the current arrangement together.
% FOUNDING_PROBLEM: The LDS Church faced institutional extinction under federal anti-polygamy legislation (Edmunds Act 1882, Edmunds-Tucker Act 1887): property seizure, disincorporation, disfranchisement, and imprisonment of leadership. The Manifesto was issued to secure institutional survival and Utah statehood.
% FOUNDING_PROBLEM_CORROBORATION: Federal legislative record (Edmunds-Tucker Act enforcement, Utah statehood 1896) corroborates the existential threat was real and resolved. The church's own official histories (e.g., 'The Manifesto and the End of Plural Marriage,' Church History Topics) acknowledge the Manifesto was issued under duress for survival. Fundamentalist splinter groups attest the founding problem (federal coercion) is gone but the accommodation persists — corroborating the 'dead' status from outside the beneficiary set.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__temporal_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__temporal_accommodation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__temporal_accommodation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(eternal_marriage_covenant__temporal_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__temporal_accommodation_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__temporal_accommodation_reading_tests).
:- end_tests(eternal_marriage_covenant__temporal_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate: the constraint extracts compliance and cognitive labor from members (living a practice they're taught is lower law) and extracts bodies/lives from fundamentalist women. Suppression (0.72) is high: the church actively excommunicates polygamists, polices temple worthiness, and disciplines scholars; fundamentalist communities enforce internal compliance through isolation and spiritual threats. Theater ratio (0.28) reflects that the 'temporary' framing is increasingly performative — the accommodation has lasted longer than the practice it suspended. Accessibility collapse (0.65) is significant: alternatives (renunciation or restoration) are doctrinally blocked. Resistance (0.4) is moderate: fundamentalists resist by continuing practice; scholars resist by critiquing; but mainstream laity largely accommodate.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional leadership seat (agenda_setter, identity_locked), the accommodation is a inspired, temporary pragmatic measure preserving the kernel for future restoration — a scaffold. From fundamentalist families (payer, trapped), it is a betrayal and ongoing extraction — a snare. From mainstream laity (beneficiary/payer, constrained), it is a necessary compromise with persistent cognitive cost — a tangled rope. From federal authorities (beneficiary, arbitrage), it is a successful coercion outcome — a rope (coordination achieved). The engine computes these per-seat divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Church leadership is identity_locked: their prophetic identity and institutional survival are fused to maintaining the kernel while managing the accommodation. They cannot exit the frame without dissolving their authority. Federal authorities have arbitrage-grade exit: they could re-escalate or ignore at will. Mainstream laity are constrained: exit means losing community, family, and salvation narrative. Fundamentalist families and coerced women are trapped: geographic, economic, and epistemic isolation prevent exit. Dissident scholars are constrained: professional and social costs of exit are high but possible. The Manifesto's 'obedience to law of land' rhetoric masks the asymmetry: the law of land was imposed by force; the accommodation was extracted, not chosen.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal existential threat) is dead — Utah statehood achieved 1896, anti-polygamy laws no longer threaten institutional survival. Yet the accommodation persists and has hardened: the church now actively enforces monogamy while teaching polygamy's eternal validity. This is mandatrophy — the scaffold's sunset condition (political constraints lifting) was met, but the structure remains because it now serves new functions: boundary maintenance against fundamentalists, identity differentiation from mainstream Christianity, and institutional control over sexuality and gender. The 'temporary' claim has become a permanent extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrine_practice_separation_ambiguity,
    'Is the distinction between ''eternal doctrine'' and ''temporal practice'' a coherent theological category or a post-hoc rationalization to preserve institutional legitimacy?',
    'Systematic theological analysis of D&C 132, the Manifesto, and subsequent official statements (e.g., 1904 Second Manifesto, 1998 Hinckley interview) testing whether the doctrine/practice distinction has stable internal criteria or expands/contracts to fit institutional needs.',
    'If coherent, the accommodation is a genuine scaffold with a defensible sunset logic. If rationalization, the constraint is a snare using doctrinal ambiguity to extract compliance while preserving claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_practice_separation_ambiguity, conceptual, 'Whether the doctrine/practice distinction is structurally stable or instrumentally elastic.').

omega_variable(
    sunset_condition_satisfaction,
    'Were the political constraints that justified the Manifesto genuinely lifted (allowing restoration), or did the church redefine the sunset condition to make it permanently unsatisfied?',
    'Historical analysis of church leadership statements 1890–present: when/if leaders declared conditions met for restoration. Compare to actual political-legal landscape (Reynolds v. US never overturned, but enforcement ceased).',
    'If conditions were met but restoration not pursued, the scaffold claim is falsified — the constraint is a piton or snare. If conditions genuinely persist, the scaffold claim holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sunset_condition_satisfaction, empirical, 'Whether the Manifesto''s stated sunset condition was ever satisfied.').

omega_variable(
    gendered_extraction_asymmetry,
    'Does the accommodation extract disproportionately from women (both historical plural wives and fundamentalist women) while distributing benefits to male-led institutional and federal actors?',
    'Demographic and historical analysis of who bears the costs (excommunication, prosecution, coercion, reproductive burden) vs. who gains (institutional survival, statehood, patriarchal authority) across the interval.',
    'If extraction is gendered, the constraint''s classification from women''s seats diverges sharply from institutional seats — a snare for women, scaffold/rope for men. This seat divergence is the measurement the framework exists to take.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gendered_extraction_asymmetry, empirical, 'Gendered distribution of extraction and benefit under the accommodation.').

omega_variable(
    kernel_reading_framing_underdetermination,
    'Does the temporal_accommodation_reading represent the only coherent framing of the Manifesto''s action, or does an alternative framing (e.g., ''prophetic surrender of the kernel'') produce a different constraint structure?',
    'Compare the structural outputs of this reading vs. a ''prophetic_surrender_reading'' where the Manifesto is read as the prophet exercising authority to modify the kernel itself (prophetic_override_reading). Test whether the two readings produce different beneficiary/victim structures, different ε, different claimed types.',
    'If alternative framing yields different structural classification, the kernel''s under-determination is live and the committer frame is analytically necessary. If both framings converge, the distinction is semantic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing_underdetermination, conceptual, 'Framing under-determination of the Manifesto''s structural meaning — whether the kernel admits multiple structurally distinct readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__temporal_accommodation_reading, 1890, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eternal_marriage_covenant_temporal_accommodation_tr_t1890, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1890, 0.15).
narrative_ontology:measurement(eternal_marriage_covenant_temporal_accommodation_tr_t1904, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1904, 0.22).
narrative_ontology:measurement(eternal_marriage_covenant_temporal_accommodation_tr_t1950, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1950, 0.28).
narrative_ontology:measurement(eternal_marriage_covenant_temporal_accommodation_tr_t1978, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1978, 0.3).
narrative_ontology:measurement(eternal_marriage_covenant_temporal_accommodation_tr_t1990, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1990, 0.27).
narrative_ontology:measurement(eternal_marriage_covenant_temporal_accommodation_tr_t2024, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(eternal_marriage_covenant_temporal_accommodation_be_t1890, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1890, 0.75).
narrative_ontology:measurement(eternal_marriage_covenant_temporal_accommodation_be_t1904, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1904, 0.65).
narrative_ontology:measurement(eternal_marriage_covenant_temporal_accommodation_be_t1950, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1950, 0.5).
narrative_ontology:measurement(eternal_marriage_covenant_temporal_accommodation_be_t1978, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1978, 0.42).
narrative_ontology:measurement(eternal_marriage_covenant_temporal_accommodation_be_t1990, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(eternal_marriage_covenant_temporal_accommodation_be_t2024, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(eternal_marriage_covenant_temporal_accommodation_su_t1890, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1890, 0.85).
narrative_ontology:measurement(eternal_marriage_covenant_temporal_accommodation_su_t1904, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1904, 0.8).
narrative_ontology:measurement(eternal_marriage_covenant_temporal_accommodation_su_t1950, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1950, 0.7).
narrative_ontology:measurement(eternal_marriage_covenant_temporal_accommodation_su_t1978, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1978, 0.65).
narrative_ontology:measurement(eternal_marriage_covenant_temporal_accommodation_su_t1990, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1990, 0.68).
narrative_ontology:measurement(eternal_marriage_covenant_temporal_accommodation_su_t2024, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__temporal_accommodation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(eternal_marriage_covenant__temporal_accommodation_reading, 0.08).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant__immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant__prophetic_override_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, mormon_temple_worthiness_gate).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, fundamentalist_mormon_splinter_formation).

% DUAL FORMULATION NOTE:
% This reading (temporal_accommodation) and the immutable_commandment_reading form a constraint family around the eternal_marriage_covenant kernel. The accommodation reading extracts from fundamentalists and women to preserve institutional legitimacy; the immutable reading extracts from the institution (excommunication, property loss) to preserve the kernel's literal practice. The prophetic_override_reading resolves the tension by relocating authority to the living prophet — a different coordination mechanism (expertise/lineage vs. extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eternal_marriage_covenant__temporal_accommodation_reading, institutional, 0.15).
constraint_indexing:directionality_override(eternal_marriage_covenant__temporal_accommodation_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
