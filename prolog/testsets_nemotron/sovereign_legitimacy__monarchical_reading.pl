% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__monarchical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__monarchical_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: sovereign_legitimacy__monarchical_reading
 *   human_readable: Monarchical Legitimacy: Divine Right & Bloodline Succession
 *   domain: political/philosophical/constitutional
 *
 * SUMMARY:
 *   This constraint story instantiates the monarchical_reading of the
 *   sovereign_legitimacy kernel: legitimate authority flows downward from the
 *   sovereign through inherited right, grounded in divine sanction,
 *   tradition, and bloodline continuity. The constraint operates as a snare —
 *   high extraction (0.82) from subjects who are excluded from authority
 *   participation, enforced through suppression of alternative legitimacy
 *   claims (0.88). Beneficiaries are the hereditary sovereign, aristocratic
 *   hierarchy, court establishment, and state church hierarchy; victims are
 *   disenfranchised subjects, excluded merchant class, reformist
 *   intellectuals, and succession rivals. The legitimating mechanism is
 *   continuity of bloodline and traditional ritual (coronation, anointing,
 *   hereditary succession). The reading relates to two sibling readings:
 *   republican_reading (popular sovereignty, authority flows upward) and
 *   constitutional_hybrid_reading (dual-sourced authority with constitutional
 *   mediation). Per the ε-invariance principle, this is a distinct constraint
 *   with its own ε, not a measurement variant.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, 0.82).
domain_priors:suppression_score(sovereign_legitimacy__monarchical_reading, 0.88).
domain_priors:theater_ratio(sovereign_legitimacy__monarchical_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__monarchical_reading, snare).
narrative_ontology:human_readable(sovereign_legitimacy__monarchical_reading, "Monarchical Legitimacy: Divine Right & Bloodline Succession").
narrative_ontology:topic_domain(sovereign_legitimacy__monarchical_reading, "political/philosophical/constitutional").

domain_priors:requires_active_enforcement(sovereign_legitimacy__monarchical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__monarchical_reading, 'f3caa1c4-2334-468f-908e-c5c9949b0cdc').
narrative_ontology:cs_kernel_codification('f3caa1c4-2334-468f-908e-c5c9949b0cdc', fixed_text).
narrative_ontology:cs_authority_grounding('f3caa1c4-2334-468f-908e-c5c9949b0cdc', lineage).
narrative_ontology:cs_interpretation_layer_present('f3caa1c4-2334-468f-908e-c5c9949b0cdc').
narrative_ontology:cs_reading_relation('f3caa1c4-2334-468f-908e-c5c9949b0cdc', sovereign_legitimacy__republican_reading, forecloses).
narrative_ontology:cs_reading_relation('f3caa1c4-2334-468f-908e-c5c9949b0cdc', sovereign_legitimacy__constitutional_hybrid_reading, influences).
narrative_ontology:cs_axiom('f3caa1c4-2334-468f-908e-c5c9949b0cdc', foundational, divine_sanction_of_bloodline_succession).
narrative_ontology:cs_axiom_status(divine_sanction_of_bloodline_succession, holdable).
narrative_ontology:cs_axiom_grounding('f3caa1c4-2334-468f-908e-c5c9949b0cdc', divine_sanction_of_bloodline_succession, theological).
narrative_ontology:cs_axiom('f3caa1c4-2334-468f-908e-c5c9949b0cdc', secondary, organic_society_requires_hereditary_hierarchy).
narrative_ontology:cs_axiom_status(organic_society_requires_hereditary_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('f3caa1c4-2334-468f-908e-c5c9949b0cdc', organic_society_requires_hereditary_hierarchy, deontological).
narrative_ontology:cs_reference_frame('f3caa1c4-2334-468f-908e-c5c9949b0cdc', dynastic_legitimacy_framework).
narrative_ontology:cs_drift_state('f3caa1c4-2334-468f-908e-c5c9949b0cdc', post_westphalian_sovereignty, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f3caa1c4-2334-468f-908e-c5c9949b0cdc', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, hereditary_sovereign).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, aristocratic_hierarchy).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, court_establishment).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, state_church_hierarchy).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, disenfranchised_subjects).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, excluded_merchant_class).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, reformist_intellectuals).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, succession_rivals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, court_establishment).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__monarchical_reading, divine_appointment_of_rulers).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__monarchical_reading, bloodline_continuity_as_legitimacy).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__monarchical_reading, organic_society_hierarchy).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__monarchical_reading, coronation_ritual_validates_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds supreme executive, legislative, and judicial authority by right of bloodline. Appoints ministers, commands armies, controls succession, and receives the surplus extracted from the realm. Justifies authority through divine sanction and coronation ritual. Can abdicate or negotiate away powers but rarely does; exit is arbitrage-grade (exile with retained claim, abdication with pension).
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, hereditary_sovereign, agenda_setter,
    institutional, generational, arbitrage, national).

% Holds hereditary titles, land, judicial privileges, military commissions, and court offices by right of bloodline. Collects rents from peasant labor and urban commerce. Depends on the sovereign for confirmation of privileges and protection from merchant-class challenges. Exit is constrained: can defect to rival claimants or flee abroad, but loses hereditary position.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, aristocratic_hierarchy, beneficiary,
    powerful, generational, constrained, national).

% Comprises royal household officers, ministers, diplomats, and favorites who administer the realm and distribute patronage. Collects salaries, perquisites, and influence. Pays the cost of constant court intrigue, dependence on sovereign's favor, and vulnerability to purges. Exit is constrained: skills are court-specific; exile means loss of position.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, court_establishment, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__monarchical_reading, court_establishment, payer).

% Performs coronation and anointing rituals that validate the sovereign's divine right. Controls education, marriage, morality policing, and poor relief. Receives tithes, land grants, and legal immunities. Depends on the sovereign for protection against dissent and heresy. Exit is constrained: schism risks loss of establishment privileges; alignment with rival claimants is dangerous.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, state_church_hierarchy, beneficiary,
    institutional, generational, constrained, national).

% Peasants, artisans, laborers who bear taxes, labor dues, military conscription, and legal disabilities without political voice. Subject to arbitrary justice, feudal obligations, and religious conformity enforcement. Exit is constrained: emigration is legally restricted and economically prohibitive; rebellion is suppressed by state violence.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, disenfranchised_subjects, payer,
    powerless, biographical, constrained, national).

% Urban merchants, bankers, professionals who generate economic surplus but are excluded from political authority and burdened with arbitrary taxation, monopolies granted to aristocrats, and guild restrictions. Their wealth gives them mobile exit (capital flight, relocation to freer cities) but their political exclusion makes them structural payers. They fund reformist intellectuals and rival claimants.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, excluded_merchant_class, payer,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__monarchical_reading, excluded_merchant_class, excluded).

% Writers, jurists, clerics who articulate alternative legitimacy (natural law, popular sovereignty, constitutionalism). Face censorship, imprisonment, exile, or execution. Their ideas cannot be fully suppressed (print, correspondence networks) but their persons are trapped — they cannot exit the intellectual battle without abandoning their vocation.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, reformist_intellectuals, excluded,
    moderate, biographical, trapped, national).

% Princes, dukes, foreign claimants with bloodline claims to the throne. Excluded by primogeniture or sovereign's designation. Bear costs of maintaining rival courts, funding rebellions, and risking attainder. Exit is constrained: renunciation is politically costly; rebellion risks death. Their exclusion is the constraint's primary vulnerability — succession contests are the moments when extraction spikes and suppression intensifies.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, succession_rivals, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__monarchical_reading, succession_rivals, excluded).

% Analyzes the constraint across historical cases and theoretical frameworks. Sees the full structure: how the divine-right premise coordinates succession but extracts from subjects, how the ritual apparatus legitimates extraction, how the constraint relates to its sibling readings. Neither collects nor pays; observes the constraint's operation across the kernel family.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, comparative_political_theorist, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sovereign_legitimacy__monarchical_reading, hereditary_sovereign).
narrative_ontology:fixing_cost_class(sovereign_legitimacy__monarchical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the succession problem in fragmented feudal orders: bloodline continuity provides a clear, non-contested (in principle) transfer of authority that prevents civil war at each transition. Also coordinates symbolic unity of the realm through the sovereign's person and ritual center.
% TRANSFER_FUNCTION: Moves political authority, legislative power, judicial supremacy, military command, and a substantial share of economic surplus (taxes, feudal dues, domain revenues) from the subject population to the hereditary sovereign and aristocratic hierarchy. The sovereign is the ultimate residual claimant.
% ABSENT_VOICES: The disenfranchised_subjects (peasants, laborers) are structurally excluded from the legitimacy conversation — they have no forum, no representation, and their objection is treated as rebellion. Reformist_intellectuals are excluded by censorship and persecution. The merchant class is excluded from authority despite generating the surplus. Women are excluded from succession in most monarchical systems (Salic law, male-preference primogeniture) — a structural exclusion within the beneficiary class itself.
% DISAPPEARANCE_RATIONALE: If the monarchical legitimacy constraint vanished overnight, the sovereign would lose legitimate authority, the aristocracy would lose hereditary privilege, the state church would lose establishment, and the subject population would face a legitimacy vacuum. The realm would reorganize around a new legitimacy ground (republican, constitutional, or warlord) — civil war, constitutional convention, or foreign intervention would follow. The world rearranges because the constraint is load-bearing for the entire political order.
% FOUNDING_PROBLEM: Feudal fragmentation and succession wars: in the absence of a clear succession rule, the death of a ruler triggered contested claims, civil war, and external invasion. Bloodline continuity grounded in divine sanction provided a single, sacralized transfer mechanism that stabilized the realm across generations.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (feudal succession wars) is attested as dead by historians of state formation (Tilly, Weber, Elias) who document the transition from feudal fragmentation to centralized bureaucratic states where succession is managed by law, not bloodline. The hereditary_sovereign and aristocratic_hierarchy attest the problem is still live, citing the need for symbolic continuity and the danger of republican chaos — but they are the beneficiaries. No corroborating source outside the beneficiary set attests the founding problem as live; the consensus of non-beneficiary scholarship is that the problem is dead.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__monarchical_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__monarchical_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__monarchical_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(sovereign_legitimacy__monarchical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__monarchical_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__monarchical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sovereign_legitimacy__monarchical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the arrangement transfers nearly all political authority and a substantial share of economic surplus to the hereditary sovereign and aristocratic hierarchy without reciprocal accountability. Suppression is very high (0.88) because the constraint's persistence depends on actively suppressing alternative legitimacy claims — republican movements, constitutional reformers, rival claimants — through legal penalties, hereditary exclusion from office, state church enforcement, and control of succession mechanisms. Theater ratio is low (0.15) because the constraint's coordination function (order, succession stability, symbolic unity) is genuine but small relative to the extraction; the ritual and tradition apparatus is not primarily performative but functional for the extraction. Accessibility collapse is moderate (0.35) because alternative legitimacy concepts (popular sovereignty, constitutionalism) remain cognitively available and historically attested, but they collapse practically under the constraint's enforcement. Resistance is high (0.72) because the excluded classes (merchant class, intellectuals, succession rivals) actively contest the arrangement throughout the interval.
 *
 * PERSPECTIVAL GAP:
 *   From the sovereign's seat (agenda_setter, institutional power, arbitrage exit), the constraint appears as a rope: genuine coordination of succession, order, and symbolic unity with minimal coercive overhead. From the disenfranchised_subjects' seat (payer, powerless, constrained exit), the same constraint computes as a snare: pure extraction enforced by suppressing alternatives. From the reformist_intellectuals' seat (excluded, moderate power, trapped exit), it computes as a snare with existential stakes. The engine computes this per-seat divergence from the structural data; the authored metrics describe the constraint's aggregate operation.
 *
 * DIRECTIONALITY LOGIC:
 *   The hereditary_sovereign and aristocratic_hierarchy are structural beneficiaries (d ≈ 0.1): they collect authority and surplus, control the rules, and have arbitrage-grade exit (can abdicate, negotiate, or flee). The court_establishment and state_church_hierarchy are secondary beneficiaries (d ≈ 0.25): they collect status and resources but depend on the sovereign's favor. Disenfranchised_subjects and excluded_merchant_class are structural targets (d ≈ 0.9): they bear the extraction (taxes, labor, political exclusion) with constrained exit (emigration is costly, rebellion is suppressed). Reformist_intellectuals and succession_rivals are high-target agents (d ≈ 0.95): they actively oppose the constraint and face severe suppression (execution, exile, attainder). The engine derives these d values from the beneficiary/victim declarations plus exit options; the override array is not needed because the structural derivation captures the relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (dynastic succession stability in fragmented feudal orders) is dead — the problem the arrangement was built for no longer exists in the contemporary world. Yet the arrangement persists in residual ceremonial forms and in authoritarian regimes that adopt monarchical legitimation. The founding_problem_status = dead with disappearance_verdict = world_rearranges creates the mandatrophy signature: a constraint whose founding problem is gone but whose removal would rearrange the world (because the extraction apparatus has become load-bearing). This is the classic zombie constraint pattern — not a piton because the administrator (the sovereign) still benefits substantially from maintaining it, but a snare whose coordination cover story has atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the monarchical reading of sovereign legitimacy a structurally distinct constraint from the republican and constitutional hybrid readings, or are they observable-dependent framings of one arrangement?',
    'Apply ε-invariance test: do the three readings author different beneficiary/victim structures, different suppression mechanisms, different extractiveness profiles? If yes, they are distinct constraints linked by network.affects_constraints; if no, they are one constraint with a measurement parameter.',
    'If distinct, each reading gets its own constraint story with its own ε and classification; the kernel_id documents the family. If not distinct, the framework must model observable-dependent classification within one story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three declared readings of sovereign_legitimacy are separate constraints or one constraint viewed from three angles').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.88) structural (legal penalties for dissent, hereditary exclusion from office, state church enforcement) or internalized (subjects believe the sovereign''s right is divinely ordained, identity fused with the dynastic order)?',
    'Post-dynastic suppression trajectory: if suppression persists after the extractive mechanism (the dynasty) is removed — e.g., subjects continue to defer to hereditary claimants — reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the subject carries the suppression with them after regime change. Affects T17 drift detection and cross-generational classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in monarchical legitimacy').

omega_variable(
    ceremonial_residue_as_scaffold,
    'Does the monarchical reading leave a residual coordination function (ceremonial unity, symbolic continuity) that persists as a scaffold after political extraction is removed, or does the entire structure dissolve when the divine-right premise is rejected?',
    'Historical comparison: track post-monarchical regimes that retained ceremonial monarchs (UK, Japan, Scandinavia) vs. those that abolished the institution entirely (France, Russia, Iran). Measure whether ceremonial retention reduces transition costs or legitimacy vacuums.',
    'If scaffold residue exists, the monarchical constraint is a constraint family: political_snare + ceremonial_scaffold. If not, it is a pure snare with no transitional function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ceremonial_residue_as_scaffold, empirical, 'Whether ceremonial monarchy functions as a scaffold after the snare is removed').

omega_variable(
    succession_contest_extraction_spike,
    'Do succession contests (war of succession, disputed heir, regency crises) function as periodic extraction spikes that reset the constraint''s theater_ratio and suppress resistance, or are they exogenous shocks?',
    'Compare extractiveness and suppression metrics in the 5-year windows before/after major succession crises across 20+ historical monarchies. Test for systematic spikes.',
    'If endogenous, the constraint''s temporal profile is cyclical with extraction peaks at succession boundaries — measurement grids must sample at succession frequency. If exogenous, the constraint is stable between external shocks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(succession_contest_extraction_spike, empirical, 'Whether succession crises are endogenous extraction cycles or exogenous shocks').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__monarchical_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sovleg_mon_tr_t0, sovereign_legitimacy__monarchical_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(sovleg_mon_tr_t50, sovereign_legitimacy__monarchical_reading, theater_ratio, 50, 0.08).
narrative_ontology:measurement(sovleg_mon_tr_t100, sovereign_legitimacy__monarchical_reading, theater_ratio, 100, 0.1).
narrative_ontology:measurement(sovleg_mon_tr_t150, sovereign_legitimacy__monarchical_reading, theater_ratio, 150, 0.12).
narrative_ontology:measurement(sovleg_mon_tr_t200, sovereign_legitimacy__monarchical_reading, theater_ratio, 200, 0.13).
narrative_ontology:measurement(sovleg_mon_tr_t250, sovereign_legitimacy__monarchical_reading, theater_ratio, 250, 0.14).
narrative_ontology:measurement(sovleg_mon_tr_t300, sovereign_legitimacy__monarchical_reading, theater_ratio, 300, 0.15).

% Extraction over time
narrative_ontology:measurement(sovleg_mon_be_t0, sovereign_legitimacy__monarchical_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(sovleg_mon_be_t50, sovereign_legitimacy__monarchical_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement(sovleg_mon_be_t100, sovereign_legitimacy__monarchical_reading, base_extractiveness, 100, 0.68).
narrative_ontology:measurement(sovleg_mon_be_t150, sovereign_legitimacy__monarchical_reading, base_extractiveness, 150, 0.73).
narrative_ontology:measurement(sovleg_mon_be_t200, sovereign_legitimacy__monarchical_reading, base_extractiveness, 200, 0.76).
narrative_ontology:measurement(sovleg_mon_be_t250, sovereign_legitimacy__monarchical_reading, base_extractiveness, 250, 0.79).
narrative_ontology:measurement(sovleg_mon_be_t300, sovereign_legitimacy__monarchical_reading, base_extractiveness, 300, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(sovleg_mon_su_t0, sovereign_legitimacy__monarchical_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(sovleg_mon_su_t50, sovereign_legitimacy__monarchical_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement(sovleg_mon_su_t100, sovereign_legitimacy__monarchical_reading, suppression_requirement, 100, 0.78).
narrative_ontology:measurement(sovleg_mon_su_t150, sovereign_legitimacy__monarchical_reading, suppression_requirement, 150, 0.82).
narrative_ontology:measurement(sovleg_mon_su_t200, sovereign_legitimacy__monarchical_reading, suppression_requirement, 200, 0.85).
narrative_ontology:measurement(sovleg_mon_su_t250, sovereign_legitimacy__monarchical_reading, suppression_requirement, 250, 0.87).
narrative_ontology:measurement(sovleg_mon_su_t300, sovereign_legitimacy__monarchical_reading, suppression_requirement, 300, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__monarchical_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sovereign_legitimacy__monarchical_reading, 0.08).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy__republican_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy__constitutional_hybrid_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, dynastic_succession_law).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, state_church_establishment).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, aristocratic_privilege_system).

% DUAL FORMULATION NOTE:
% This constraint is the monarchical_reading of the sovereign_legitimacy kernel. The republican_reading and constitutional_hybrid_reading are sibling constraints with different ε, different beneficiary/victim structures, and different suppression mechanisms. All three are linked via network.affects_constraints. The monarchical reading forecloses the republican reading's core premise (popular sovereignty) within any single framework — they cannot coexist as the ground of legitimacy. The monarchical reading influences the constitutional_hybrid reading by providing the inherited/ceremonial pole that the hybrid mediates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
