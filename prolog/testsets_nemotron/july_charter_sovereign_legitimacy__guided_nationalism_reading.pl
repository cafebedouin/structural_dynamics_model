% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__guided_nationalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__guided_nationalism_reading, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__guided_nationalism_reading
 *   human_readable: July Charter Sovereign Legitimacy — Guided Nationalism Reading
 *   domain: constitutional_law/political_transitions/post_revolutionary_state_building
 *
 * SUMMARY:
 *   The July Charter (modeled on Iran's 1979 Constitution) establishes an
 *   Islamic-nationalist framework where religious identity is the ground of
 *   sovereign legitimacy. The guided_nationalism_reading instantiates the
 *   constraint that the charter's religious ground is a genuine coordination
 *   mechanism for the revolutionary coalition AND an extractive structure
 *   against secular civil society, religious minorities, and democratic
 *   opposition. This is one reading of the contested kernel
 *   'july_charter_sovereign_legitimacy' — the secular_democratic_reading and
 *   military_custodian_reading instantiate different constraints from the
 *   same text.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.68).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.75).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__guided_nationalism_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__guided_nationalism_reading, "July Charter Sovereign Legitimacy — Guided Nationalism Reading").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__guided_nationalism_reading, "constitutional_law/political_transitions/post_revolutionary_state_building").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__guided_nationalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__guided_nationalism_reading, '0ae6df3c-53fb-48cf-a0e5-799d32bca7bb').
narrative_ontology:cs_kernel_codification('0ae6df3c-53fb-48cf-a0e5-799d32bca7bb', fixed_text).
narrative_ontology:cs_authority_grounding('0ae6df3c-53fb-48cf-a0e5-799d32bca7bb', lineage).
narrative_ontology:cs_interpretation_layer_present('0ae6df3c-53fb-48cf-a0e5-799d32bca7bb').
narrative_ontology:cs_reading_relation('0ae6df3c-53fb-48cf-a0e5-799d32bca7bb', july_charter_sovereign_legitimacy__secular_democratic_reading, forecloses).
narrative_ontology:cs_reading_relation('0ae6df3c-53fb-48cf-a0e5-799d32bca7bb', july_charter_sovereign_legitimacy__military_custodian_reading, coexists_with).
narrative_ontology:cs_axiom('0ae6df3c-53fb-48cf-a0e5-799d32bca7bb', foundational, religious_identity_as_sovereign_source).
narrative_ontology:cs_axiom_status(religious_identity_as_sovereign_source, holdable).
narrative_ontology:cs_axiom_grounding('0ae6df3c-53fb-48cf-a0e5-799d32bca7bb', religious_identity_as_sovereign_source, theological).
narrative_ontology:cs_axiom('0ae6df3c-53fb-48cf-a0e5-799d32bca7bb', foundational, guardian_institutions_as_divine_trustees).
narrative_ontology:cs_axiom_status(guardian_institutions_as_divine_trustees, holdable).
narrative_ontology:cs_axiom_grounding('0ae6df3c-53fb-48cf-a0e5-799d32bca7bb', guardian_institutions_as_divine_trustees, theological).
narrative_ontology:cs_axiom('0ae6df3c-53fb-48cf-a0e5-799d32bca7bb', secondary, islamic_governance_necessity).
narrative_ontology:cs_axiom_status(islamic_governance_necessity, holdable).
narrative_ontology:cs_axiom_grounding('0ae6df3c-53fb-48cf-a0e5-799d32bca7bb', islamic_governance_necessity, deontological).
narrative_ontology:cs_reference_frame('0ae6df3c-53fb-48cf-a0e5-799d32bca7bb', velayat_e_faqih_constitutional_order).
narrative_ontology:cs_drift_state('0ae6df3c-53fb-48cf-a0e5-799d32bca7bb', contemporary_post_reform_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0ae6df3c-53fb-48cf-a0e5-799d32bca7bb', '2026-08-03T14:22:17Z').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_nationalist_movement).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, clerical_establishment).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, ideological_guardian_institutions).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minorities).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, democratic_opposition_parties).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, independent_judiciary).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_identity_as_sovereign_source).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__guided_nationalism_reading, guided_democracy_doctrine).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__guided_nationalism_reading, islamic_governance_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and championed the charter's Islamic-nationalist framework; controls the constitutional interpretation machinery through guardian institutions (Supreme Leader office, Guardian Council equivalents). Their political identity is fused to the charter's legitimacy — abandoning the framework would dissolve their reason for existence as a movement. They extract constitutional authority and resource control from the arrangement.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_nationalist_movement, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_nationalist_movement, beneficiary).

% Gains constitutional recognition of religious authority over legislation, personal status law, and public morality. Seminaries and clerical networks receive state funding and regulatory privileges. Their institutional survival depends on the charter's religious ground — secularization would strip their legal standing. They benefit from the constraint but do not administer it directly.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, clerical_establishment, beneficiary,
    organized, generational, identity_locked, national).

% Guardian Council, Supreme Leader office, revolutionary courts — these institutions wield veto power over legislation, candidacies, and judicial appointments to enforce conformity with the Islamic-nationalist framework. Their institutional mandate and budget depend entirely on the charter's religious ground. They are trapped: the institutions cannot exist in their current form under any alternative constitutional order.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, ideological_guardian_institutions, agenda_setter,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__guided_nationalism_reading, ideological_guardian_institutions, beneficiary).

% Human rights NGOs, women's organizations, labor unions, independent journalists, academics — their activities are constrained by religious-law-based restrictions on assembly, expression, and gender equality. They bear compliance costs, censorship, and criminalization risk. Exit is constrained: emigration is possible but costly; internal dissent carries prison risk. They pay through lost freedoms and organizational suppression.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society, payer,
    moderate, biographical, constrained, national).

% Christians, Jews, Zoroastrians, Baha'is, Sunni Muslims in Shia-dominated framework, Sufi orders — face legal disabilities in personal status, worship, education, and public office. Baha'is face criminalization. Exit is nearly impossible: emigration barriers, communal ties, and identity-locked belonging make flight a last resort. They bear the heaviest extraction with the least exit.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minorities, payer,
    powerless, biographical, trapped, national).

% Reformist and secular parties allowed to exist but barred from meaningful power by guardian institution vetting. They pay through co-optation pressure, leadership imprisonment, and electoral irrelevance. Their exit is constrained: boycott cedes ground; participation legitimizes the system. They are structurally excluded from the constitutional conversation despite being the primary organized alternative.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, democratic_opposition_parties, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__guided_nationalism_reading, democratic_opposition_parties, excluded).

% Judges trained in secular legal tradition but bound to apply religious-law-derived statutes and guardian institution directives. Judicial independence is structurally impossible under the charter's framework. They cannot exit the role without leaving the profession; they cannot reform it from within. They are excluded from constitutional interpretation authority.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, independent_judiciary, excluded,
    powerless, biographical, trapped, national).

% UN treaty bodies, special rapporteurs, regional human rights courts — monitor and document violations but lack enforcement leverage. Their analytical seat sees the full structure: the charter's religious ground as both coordination mechanism for the revolutionary coalition and extraction mechanism against minorities and dissenters.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the post-revolutionary legitimacy vacuum by anchoring sovereignty in a shared Islamic-nationalist identity, providing a unifying framework that binds the revolutionary coalition (clerics, nationalist militia, traditional bazaar merchants) against fragmentation, foreign intervention, and counter-revolutionary forces.
% TRANSFER_FUNCTION: Moves constitutional interpretation authority, legislative veto power, control over personal status law and public morality enforcement, state resource allocation to clerical networks, and the power to define political eligibility from the sovereign people to the religious-nationalist guardian institutions. The transfer runs from secular civil society, religious minorities, and democratic opposition → clerical establishment and guardian institutions.
% ABSENT_VOICES: Religious minorities (especially Baha'is, criminalized entirely), women's rights advocates demanding full legal equality, secular constitutionalists arguing for popular sovereignty, independent labor organizers, and the pre-revolutionary professional classes who fled — these voices are structurally excluded by the charter's voter/vetting mechanisms and criminalization of dissent. They would object to the religious ground of legitimacy and the extraction it enables, but they are not in the constitutional conversation.
% DISAPPEARANCE_RATIONALE: If the Islamic-nationalist ground vanished overnight, the guardian institutions would lose their constitutional mandate, the legislative veto would collapse, personal status law would revert to contested pluralism, the revolutionary coalition's unity would fracture, and a constituent power struggle would erupt between secular democrats, military custodians, and religious nationalists — the entire post-revolutionary constitutional order would rearrange.
% FOUNDING_PROBLEM: The 1979 revolution overthrew a secular monarchy but faced immediate fragmentation: clerical-royalist-nationalist-leftist coalition had no shared governance model, foreign invasion (Iran-Iraq war) loomed, and the state apparatus was collapsing. The charter was built to solve the immediate problem of sovereign legitimacy and coalition cohesion in an existential crisis.
% FOUNDING_PROBLEM_CORROBORATION: The religious-nationalist movement attests the founding problem remains live (ongoing foreign pressure, cultural imperialism, internal subversion). Secular democrats and religious minorities attest the founding problem is dead (the war ended 1988, the state is consolidated, the charter now serves entrenchment). International constitutional scholars (independent of beneficiary parties) corroborate the shifted-function reading: the existential crisis has passed but the emergency framework persists.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__guided_nationalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__guided_nationalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__guided_nationalism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__guided_nationalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__guided_nationalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__guided_nationalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__guided_nationalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68) is substantial because the charter transfers sovereign authority from popular sovereignty to unelected guardian institutions, enabling resource capture by clerical networks and ideological enforcement. Suppression (0.75) is high because the constraint actively excludes rival sovereignty claims (secular democracy, military custodianship) and criminalizes dissent from the religious ground. Theater ratio (0.42) is moderate and rising — early revolutionary tribunals had genuine coordination function; contemporary guardian institution activity increasingly performs ideological conformity rather than solving coordination problems. Accessibility collapse (0.65) reflects that alternatives (secular constitution, pluralist democracy) are conceptually available but structurally blocked by the guardian veto. Resistance (0.58) is significant but fragmented — protest cycles (1999, 2009, 2017, 2022) show persistent opposition but no successful exit.
 *
 * PERSPECTIVAL GAP:
 *   From the religious_nationalist_movement and ideological_guardian_institutions seats (agenda_setter/beneficiary, identity_locked), the constraint computes as rope/scaffold — genuine coordination solving the founding problem. From secular_civil_society, religious_minorities, and democratic_opposition_parties seats (payer/excluded, constrained/trapped), the same constraint computes as snare/tangled_rope — enforced extraction with suppressed exits. The independent_judiciary (excluded, trapped) experiences it as piton — a degraded institution maintaining theatrical independence. The engine computes this seat divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The religious_nationalist_movement and clerical_establishment are structural beneficiaries (d near 0.0) — they collect constitutional authority, resource control, and institutional survival from the constraint. The ideological_guardian_institutions are agenda_setters with identity_locked exit (d ~ 0.15) — they administer and benefit but cannot exit without institutional suicide. Secular_civil_society and democratic_opposition_parties are payers with constrained exit (d ~ 0.7) — they bear costs but retain limited voice. Religious_minorities and independent_judiciary are payers/excluded with trapped exit (d ~ 0.9) — they bear maximum extraction with near-zero exit. International observers are analytical (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-revolutionary legitimacy vacuum + existential war) is contested — the religious-nationalist movement claims it persists; secular democrats and minorities say it ended decades ago. The charter's mandate has partially atrophied: the coordination function (coalition cohesion against existential threat) has degraded as the state consolidated, but the extraction function (guardian institution privileges, clerical resource capture, minority suppression) persists and has expanded. This is the tangled_rope signature: genuine coordination origin, asymmetric extraction now dominant, active enforcement required to prevent exit. The mandatrophy is unresolved — the arrangement has not acknowledged the shift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_frame_location,
    'Which structural element of the charter do the sibling readings genuinely disagree on — the text''s semantic content, the authority structure''s grounding, or the legitimacy claim''s referent?',
    'Comparative textual analysis of each reading''s citation pattern: which articles, clauses, or silences each reading treats as authoritative vs. contingent. If readings cite disjoint textual bases, the disagreement is textual. If they cite the same text but differ on authority_grounding, the disagreement is structural.',
    'If textual: the kernel is under-specified (distributed codification). If structural: the kernel is fixed_text but authority_grounding is contested (lineage vs extraction vs expertise). This determines whether the kernel itself is ambiguous or whether the readings are competing authority claims over a stable kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_location, conceptual, 'Location of the committer-frame disagreement within the kernel''s structure').

omega_variable(
    founding_problem_expiry,
    'At what point did the existential founding problem (war, state collapse, coalition fragmentation) cease to justify the emergency constitutional framework, if ever?',
    'Historical analysis of threat perception documents: when did the revolutionary leadership''s internal communications stop citing existential external threat as the primary justification for guardian institutions? Correlate with measurable state capacity indicators (territorial control, monopoly on violence, administrative reach).',
    'If a clear expiry point exists (e.g., 1988 war end, 1990s state consolidation), the charter''s persistence past that point is mandatrophy — the coordination function expired but the extraction function remained. If no expiry point exists (ongoing threat perception), the coordination function may still be live and the tangled_rope classification reflects genuine hybrid function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_expiry, empirical, 'Whether the founding problem has a identifiable expiry date').

omega_variable(
    separability_of_coordination_and_extraction,
    'Can the charter''s coordination function (coalition cohesion, sovereign legitimacy) be separated from its extraction function (guardian veto, minority suppression, clerical resource capture) without collapsing the framework?',
    'Counterfactual institutional design: could a revised charter retain the Islamic-nationalist legitimacy ground and guardian institutions'' coordination role while removing the legislative veto, establishing independent judiciary, and guaranteeing minority rights? Test against the movement''s own reformist faction proposals (e.g., 1997-2005 reform era).',
    'If separable: the extraction is not structurally necessary to the coordination — the constraint is a snare wearing a rope''s coat. If inseparable: the religious ground of legitimacy structurally requires the guardian veto and minority suppression — the tangled_rope is the only stable form. This determines whether reform is possible within the kernel or requires kernel replacement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(separability_of_coordination_and_extraction, conceptual, 'Whether coordination and extraction are structurally separable in this constraint').

omega_variable(
    identity_locked_mechanism,
    'What specific identity-fusion mechanism binds the religious_nationalist_movement and clerical_establishment to the charter — professional identity (career path dependence), relational identity (self-constituted through the revolutionary relationship), ideological identity (worldview making exit unthinkable), or institutional identity (organization has ''become'' its function)?',
    'Interview-based study of defector/reformist narratives: when insiders attempt exit, what identity barrier stops them? Track career trajectories of clerics who entered politics vs. those who remained in seminaries. Analyze rhetoric of ''betrayal'' vs. ''reform'' in internal disputes.',
    'If ideological identity: the constraint''s persistence depends on belief, making it vulnerable to ideological fracture. If institutional identity: the constraint persists until the institutions themselves are replaced (harder). If professional identity: exit is possible but costly (career capital loss). This affects the directionality derivation for identity_locked agents and the predicted fracture points.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_mechanism, empirical, 'Mechanism of identity lock for beneficiary/agenda-setter agents').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_charter_gn_tr_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(july_charter_gn_tr_t9, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 9, 0.22).
narrative_ontology:measurement(july_charter_gn_tr_t18, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 18, 0.3).
narrative_ontology:measurement(july_charter_gn_tr_t27, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 27, 0.36).
narrative_ontology:measurement(july_charter_gn_tr_t36, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 36, 0.4).
narrative_ontology:measurement(july_charter_gn_tr_t45, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 45, 0.42).

% Extraction over time
narrative_ontology:measurement(july_charter_gn_be_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(july_charter_gn_be_t9, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 9, 0.52).
narrative_ontology:measurement(july_charter_gn_be_t18, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 18, 0.58).
narrative_ontology:measurement(july_charter_gn_be_t27, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 27, 0.63).
narrative_ontology:measurement(july_charter_gn_be_t36, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 36, 0.66).
narrative_ontology:measurement(july_charter_gn_be_t45, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 45, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(july_charter_gn_su_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(july_charter_gn_su_t9, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 9, 0.68).
narrative_ontology:measurement(july_charter_gn_su_t18, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 18, 0.7).
narrative_ontology:measurement(july_charter_gn_su_t27, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 27, 0.72).
narrative_ontology:measurement(july_charter_gn_su_t36, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 36, 0.74).
narrative_ontology:measurement(july_charter_gn_su_t45, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 45, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__guided_nationalism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.08).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy__secular_democratic_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy__military_custodian_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, guardian_council_legislative_veto).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, clerical_resource_allocation_network).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minority_legal_disabilities).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, revolutionary_court_jurisdiction).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'july_charter_sovereign_legitimacy' kernel. The secular_democratic_reading instantiates a rope/scaffold constraint (coordination without extraction); the military_custodian_reading instantiates a snare/piton constraint (extraction without genuine coordination). This guided_nationalism_reading instantiates the tangled_rope — the hybrid case where the charter's Islamic-nationalist ground provides genuine coordination for the revolutionary coalition AND asymmetric extraction against secular civil society and religious minorities. The three readings are not the same constraint viewed differently — they have different ε values, different beneficiary/victim structures, and different structural dynamics. The kernel label 'the July Charter' conflates them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(july_charter_sovereign_legitimacy__guided_nationalism_reading, institutional, 0.1).
constraint_indexing:directionality_override(july_charter_sovereign_legitimacy__guided_nationalism_reading, organized, 0.15).
constraint_indexing:directionality_override(july_charter_sovereign_legitimacy__guided_nationalism_reading, moderate, 0.7).
constraint_indexing:directionality_override(july_charter_sovereign_legitimacy__guided_nationalism_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
