% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__secular_democratic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__secular_democratic_reading, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__secular_democratic_reading
 *   human_readable: July Charter Secular Democratic Mandate with Military Subordination
 *   domain: constitutional_law/political_transitions/post_revolutionary_state_building
 *
 * SUMMARY:
 *   The July Charter, adopted after the 2024 mass uprising that toppled the
 *   authoritarian regime, establishes a secular democratic constitutional
 *   order with explicit military subordination to civilian authority. The
 *   secular democratic reading — championed by the revolutionary student
 *   movement, progressive parties, and civil society — frames the charter as
 *   a genuine democratic transition instrument. It coordinates a real
 *   collective-action problem (preventing both military tutelary coups and
 *   Islamist majoritarian capture) but extracts asymmetrically from two
 *   identifiable groups: Jamaat-e-Islami (banned from politics) and the
 *   military institution (stripped of autonomous political authority). The
 *   constraint requires active enforcement through constitutional court
 *   rulings, civilian control of defense ministry, and electoral commission
 *   exclusions. The claim/metric gap is deliberate: the charter is CLAIMED as
 *   tangled_rope (coordination + necessary exclusion) while the authored
 *   metrics describe substantially extractive operation — the engine measures
 *   that divergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.68).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.72).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__secular_democratic_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__secular_democratic_reading, "July Charter Secular Democratic Mandate with Military Subordination").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__secular_democratic_reading, "constitutional_law/political_transitions/post_revolutionary_state_building").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__secular_democratic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__secular_democratic_reading, '8c7832ae-97f1-456c-9f0a-867697b0147e').
narrative_ontology:cs_kernel_codification('8c7832ae-97f1-456c-9f0a-867697b0147e', formalized).
narrative_ontology:cs_authority_grounding('8c7832ae-97f1-456c-9f0a-867697b0147e', lineage).
narrative_ontology:cs_interpretation_layer_present('8c7832ae-97f1-456c-9f0a-867697b0147e').
narrative_ontology:cs_reading_relation('8c7832ae-97f1-456c-9f0a-867697b0147e', july_charter_sovereign_legitimacy__guided_nationalism_reading, forecloses).
narrative_ontology:cs_reading_relation('8c7832ae-97f1-456c-9f0a-867697b0147e', july_charter_sovereign_legitimacy__military_custodian_reading, forecloses).
narrative_ontology:cs_axiom('8c7832ae-97f1-456c-9f0a-867697b0147e', foundational, secularism_as_constitutional_principle).
narrative_ontology:cs_axiom_status(secularism_as_constitutional_principle, holdable).
narrative_ontology:cs_axiom_grounding('8c7832ae-97f1-456c-9f0a-867697b0147e', secularism_as_constitutional_principle, conventional).
narrative_ontology:cs_axiom('8c7832ae-97f1-456c-9f0a-867697b0147e', foundational, civilian_supremacy_over_military).
narrative_ontology:cs_axiom_status(civilian_supremacy_over_military, holdable).
narrative_ontology:cs_axiom_grounding('8c7832ae-97f1-456c-9f0a-867697b0147e', civilian_supremacy_over_military, conventional).
narrative_ontology:cs_axiom('8c7832ae-97f1-456c-9f0a-867697b0147e', secondary, political_islam_exclusion_as_secular_requirement).
narrative_ontology:cs_axiom_status(political_islam_exclusion_as_secular_requirement, holdable).
narrative_ontology:cs_axiom_grounding('8c7832ae-97f1-456c-9f0a-867697b0147e', political_islam_exclusion_as_secular_requirement, conventional).
narrative_ontology:cs_reference_frame('8c7832ae-97f1-456c-9f0a-867697b0147e', revolutionary_legitimacy_framework).
narrative_ontology:cs_drift_state('8c7832ae-97f1-456c-9f0a-867697b0147e', contemporary_post_transition, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8c7832ae-97f1-456c-9f0a-867697b0147e', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_democratic_forces).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, civilian_political_actors).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, jamaat_e_islami).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, military_autonomous_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, military_institution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Civil society coalitions, progressive parties, and professional associations that drove the revolution. They gain constitutional recognition for secular democratic principles and a legal framework to contest military tutelary politics. Their exit is constrained by revolutionary commitment — leaving the charter means abandoning the transition they led.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_democratic_forces, beneficiary,
    organized, biographical, constrained, national).

% The interim or elected civilian government that administers the charter. It appoints constitutional court judges, controls defense ministry appointments, and sets the legislative agenda. It benefits institutionally from the charter's mandate but faces pressure from both military and Islamist opposition. Can exit via resignation or electoral defeat.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, civilian_executive, agenda_setter,
    institutional, biographical, mobile, national).

% The primary Islamist political party, banned or severely restricted under the charter's secularism provisions. Its organizational infrastructure, electoral participation, and political representation are legally suppressed. It bears the cost of exclusion — loss of parliamentary seats, criminalization of leadership, freezing of assets. Exit means dissolving or going underground; identity_locked dynamics make this structurally improbable.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, jamaat_e_islami, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__secular_democratic_reading, jamaat_e_islami, excluded).

% The armed forces as an institution, historically accustomed to tutelary political role and autonomous control over defense budgets, appointments, and internal affairs. The charter subordinates it to civilian defense ministry, subjects promotions to civilian approval, and subjects officers to civilian courts for political crimes. It bears extraction through loss of institutional autonomy and political veto power. Exit would mean coup or institutional fracture — constrained by chain of command and institutional identity.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, military_institution, payer,
    institutional, generational, constrained, national).

% UN missions, EU observation teams, NGOs (IDEA, Carter Center, IFES) that monitor constitutional compliance, election integrity, and human rights. They provide technical assistance and legitimacy certification. Their analysis shapes donor conditionality but they lack enforcement power. Exit is analytical — they can withdraw observation missions but the constraint persists.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, international_democracy_observers, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages post-revolutionary transition to democratic governance by establishing civilian supremacy over the military and defining the political space as secular, preventing both military tutelary politics and Islamist electoral capture.
% TRANSFER_FUNCTION: Moves constitutional authority from revolutionary military council to civilian institutions; moves control over defense policy, security appointments, and legislative agenda from military to elected government; moves political participation rights away from Islamist parties toward secular forces.
% ABSENT_VOICES: Mid-ranking military officers who believe in the military's guardian role but lack institutional voice; Islamist voters (estimated 25-30% of electorate) who see secularism as foreign imposition and their exclusion as disenfranchisement; both groups are structurally excluded from the charter's founding coalition and have no formal representation in the constitutional court or transition bodies.
% DISAPPEARANCE_RATIONALE: If the charter vanished overnight, the military would reassert its tutelary role within weeks (historical precedent: 1975, 1982, 2007), Islamist parties would re-enter parliamentary politics and likely win significant seats, and the civilian government would lose its legal basis for controlling defense appointments. The post-revolutionary settlement would collapse into the pre-transition power balance.
% FOUNDING_PROBLEM: Post-revolutionary vacuum after authoritarian collapse required a legitimate framework to transfer power from military to civilians while preventing Islamist electoral dominance that the revolutionary coalition viewed as existential threat to secular democratic project.
% FOUNDING_PROBLEM_CORROBORATION: International election observers (EU EOM 2024, Carter Center 2023) and domestic human rights groups (Odhikar, Ain o Salish Kendra) corroborate the transition narrative and document military withdrawal from politics. Jamaat-e-Islami leadership and military retirees' associations contest the founding problem, arguing the charter was designed to entrench secular elite privilege. No neutral arbiter exists — the constitutional court itself is a charter creation.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__secular_democratic_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__secular_democratic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__secular_democratic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__secular_democratic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__secular_democratic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__secular_democratic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68) is substantial because the charter permanently excludes a major political actor (Jamaat-e-Islami, ~25% electoral base) and institutionally subordinates the military, transferring authority without compensation. Suppression (0.72) is high because persistence depends on active enforcement: constitutional court bans, military personnel vetting, electoral commission disqualifications. Theater (0.38) is moderate — democratic rituals (elections, parliamentary sessions) are real but the excluded groups cannot meaningfully participate. Resistance (0.75) is high from both military (institutional pushback on promotions, budget autonomy) and Islamist mobilization (street protests, international lobbying). Accessibility collapse (0.58) is moderate — alternative constitutional visions exist (guided nationalism, military custodianship) but are legally and politically marginalized.
 *
 * PERSPECTIVAL GAP:
 *   The civilian executive and secular forces experience the constraint as genuine coordination solving the transition problem. The military and Jamaat-e-Islami experience it as enforced exclusion. The engine computes this divergence from structural data: different power atoms (institutional vs organized), different exit options (mobile/constrained vs constrained/identity_locked), different spatial scopes (all national but different institutional reach). The authored claim (tangled_rope) does not adjudicate — the engine measures per-seat divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Civilian executive is the structural beneficiary (d ≈ 0.15) — controls the constraint's administration, appoints interpreters, collects institutional authority. Secular democratic forces are beneficiaries (d ≈ 0.25) — gain political space and legal tools but remain vulnerable to military/Islamist backlash. Jamaat-e-Islami is full target (d ≈ 0.95) — identity_locked, politically existential extraction, no viable exit. Military institution is high target (d ≈ 0.85) — institutional identity fused with tutelary role, constrained exit, bears direct institutional subordination. International observers are analytical (d = 0.5) — symmetric costs/benefits, no structural extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The charter's founding problem (post-revolutionary vacuum) is contested as live/dead. Secular forces argue transition incomplete (military still holds economic interests, Islamist social base intact). Islamists argue founding problem was exclusionary from inception. Military argues custodial role still needed for stability. The mandatrophy risk is high: if founding problem is dead but constraint persists with high extraction, it becomes piton or snare. Current trajectory shows rising extraction and suppression — coordination function may be atrophying into extraction maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'This constraint is one reading (secular_democratic_reading) of the contested kernel ''july_charter_sovereign_legitimacy''. What would the sibling readings (guided_nationalism_reading, military_custodian_reading) change structurally in beneficiary/victim sets and extraction profiles?',
    'Author separate constraint stories for each sibling reading with their own ε, stakeholders, and claimed types. Compare computed per-seat classifications across the family. The kernel''s ε-invariance test: if changing the reading changes ε substantially, they are distinct constraints correctly decomposed.',
    'If sibling readings produce fundamentally different victim sets (e.g., military_custodian_reading has civilians as victims, secular forces as excluded), the kernel decomposition is validated. If they produce similar structural profiles, the kernel may be a false unity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Kernel/reading decomposition structural delta').

omega_variable(
    secularism_exclusion_boundary,
    'Is the exclusion of political Islam (Jamaat-e-Islami) a necessary structural feature of secular democratic coordination, or is it sectarian extraction using secularism as cover?',
    'Comparative analysis: do other post-authoritarian secular transitions (Tunisia 2014, Turkey 1961/1982, Indonesia 1998) exclude Islamist parties permanently or integrate them? If permanent exclusion correlates with democratic durability, it may be coordination; if it correlates with democratic backsliding, it may be extraction.',
    'If exclusion is coordination-necessary, the constraint''s extraction is the price of democratic stability (tangled_rope). If exclusion is extractive cover, the constraint is snare with secularism as legitimating narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secularism_exclusion_boundary, empirical, 'Whether Islamist exclusion is coordination cost or extractive rent').

omega_variable(
    military_subordination_durability,
    'Will civilian control over the military consolidate (extraction decreasing) or will the military reassert tutelary autonomy through informal channels (extraction persistent, suppression rising)?',
    'Track military economic holdings, promotion patterns, internal accountability cases, and coup-attempt frequency over 5-10 years. If military accepts civilian budgetary control and judicial oversight without resistance, subordination consolidates. If parallel command structures persist, extraction becomes institutionalized.',
    'If subordination consolidates, theater_ratio falls and extraction drops → constraint evolves toward rope. If military captures civilian institutions informally, suppression rises and extraction persists → constraint evolves toward snare or piton.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(military_subordination_durability, empirical, 'Whether military subordination is structural or performative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__secular_democratic_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_charter_secular_democratic_tr_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(july_charter_secular_democratic_tr_t6, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 6, 0.28).
narrative_ontology:measurement(july_charter_secular_democratic_tr_t12, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(july_charter_secular_democratic_tr_t18, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 18, 0.35).
narrative_ontology:measurement(july_charter_secular_democratic_tr_t24, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement(july_charter_secular_democratic_tr_t30, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 30, 0.37).
narrative_ontology:measurement(july_charter_secular_democratic_tr_t36, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 36, 0.38).

% Extraction over time
narrative_ontology:measurement(july_charter_secular_democratic_be_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(july_charter_secular_democratic_be_t6, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(july_charter_secular_democratic_be_t12, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(july_charter_secular_democratic_be_t18, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 18, 0.62).
narrative_ontology:measurement(july_charter_secular_democratic_be_t24, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(july_charter_secular_democratic_be_t30, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 30, 0.67).
narrative_ontology:measurement(july_charter_secular_democratic_be_t36, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 36, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(july_charter_secular_democratic_su_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(july_charter_secular_democratic_su_t6, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 6, 0.61).
narrative_ontology:measurement(july_charter_secular_democratic_su_t12, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 12, 0.66).
narrative_ontology:measurement(july_charter_secular_democratic_su_t18, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 18, 0.69).
narrative_ontology:measurement(july_charter_secular_democratic_su_t24, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 24, 0.71).
narrative_ontology:measurement(july_charter_secular_democratic_su_t30, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(july_charter_secular_democratic_su_t36, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 36, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__secular_democratic_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.12).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy__guided_nationalism_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy__military_custodian_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the July Charter's sovereign legitimacy claim into three structurally distinct readings. The secular democratic reading (this story) has ε=0.68 with victims {jamaat_e_islami, military_autonomous_authority}. The guided_nationalism_reading would have victims {secular_forces, minorities} and beneficiaries {islamist_parties}. The military_custodian_reading would have victims {civilian_politicians, parliament} and beneficiaries {military_institution}. Each reading's ε differs because the coordination/extraction boundary shifts with the legitimacy ground.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(july_charter_sovereign_legitimacy__secular_democratic_reading, institutional, 0.15).
constraint_indexing:directionality_override(july_charter_sovereign_legitimacy__secular_democratic_reading, organized, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
