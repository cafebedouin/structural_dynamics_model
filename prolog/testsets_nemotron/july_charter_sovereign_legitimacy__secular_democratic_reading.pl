% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__secular_democratic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
 *   human_readable: July Charter Secular Democratic Mandate (Secular Reading)
 *   domain: constitutional_law/political_transitions
 *
 * SUMMARY:
 *   The July Charter (post-2024 revolutionary charter) is a contested
 *   constitutional text. This constraint story instantiates the
 *   secular_democratic_reading: the Charter mandates secular democratic
 *   institutions with military subordination to civilian authority. Under
 *   this reading, the Charter's coordination function is stabilizing a
 *   pluralistic democratic transition by subordinating the military and
 *   establishing secularism as state principle. Its extraction function
 *   operates through the exclusion of Political Islam actors
 *   (Jamaat-e-Islami, allied civil society) from legitimate political
 *   participation and the constraint of military autonomous authority. The
 *   other two readings of this kernel — guided_nationalism_reading
 *   (Islamic-nationalist framework) and military_custodian_reading (military
 *   as permanent guardian) — instantiate different constraints with different
 *   beneficiary/victim structures and different ε values. This story covers
 *   only the secular democratic reading.
 *
 * KEY AGENTS:
 *   - secular_democratic_parties: Primary beneficiary (organized/mobile) — gains political space and institutional protection
 *   - civilian_government_institutions: Primary beneficiary (institutional/constrained) — gains formal supremacy over military
 *   - liberal_civil_society: Secondary beneficiary (organized/mobile) — gains rights framework and protections
 *   - jamaat_e_islami_political_wing: Primary victim (organized/constrained) — excluded from political participation, faces legal suppression
 *   - military_autonomous_authority: Primary victim (institutional/constrained) — loses institutional autonomy, subordinate to civilian control
 *   - islamist_civil_society_organizations: Secondary victim (organized/constrained) — restricted operating space, surveillance
 *   - analytical_observers: Observer (analytical/analytical) — constitutional scholars, international monitors
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.72).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.78).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__secular_democratic_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__secular_democratic_reading, "July Charter Secular Democratic Mandate (Secular Reading)").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__secular_democratic_reading, "constitutional_law/political_transitions").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__secular_democratic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__secular_democratic_reading, 'e6844ac2-aca3-4ea4-bb65-d194ba8571dc').
narrative_ontology:cs_kernel_codification('e6844ac2-aca3-4ea4-bb65-d194ba8571dc', formalized).
narrative_ontology:cs_authority_grounding('e6844ac2-aca3-4ea4-bb65-d194ba8571dc', extraction).
narrative_ontology:cs_interpretation_layer_present('e6844ac2-aca3-4ea4-bb65-d194ba8571dc').
narrative_ontology:cs_reading_relation('e6844ac2-aca3-4ea4-bb65-d194ba8571dc', july_charter_sovereign_legitimacy__guided_nationalism_reading, forecloses).
narrative_ontology:cs_reading_relation('e6844ac2-aca3-4ea4-bb65-d194ba8571dc', july_charter_sovereign_legitimacy__military_custodian_reading, forecloses).
narrative_ontology:cs_axiom('e6844ac2-aca3-4ea4-bb65-d194ba8571dc', foundational, secularism_as_state_principle).
narrative_ontology:cs_axiom_status(secularism_as_state_principle, holdable).
narrative_ontology:cs_axiom_grounding('e6844ac2-aca3-4ea4-bb65-d194ba8571dc', secularism_as_state_principle, conventional).
narrative_ontology:cs_axiom('e6844ac2-aca3-4ea4-bb65-d194ba8571dc', foundational, civilian_supremacy_over_military).
narrative_ontology:cs_axiom_status(civilian_supremacy_over_military, holdable).
narrative_ontology:cs_axiom_grounding('e6844ac2-aca3-4ea4-bb65-d194ba8571dc', civilian_supremacy_over_military, conventional).
narrative_ontology:cs_axiom('e6844ac2-aca3-4ea4-bb65-d194ba8571dc', secondary, democratic_legitimacy_requires_islamist_exclusion).
narrative_ontology:cs_axiom_status(democratic_legitimacy_requires_islamist_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('e6844ac2-aca3-4ea4-bb65-d194ba8571dc', democratic_legitimacy_requires_islamist_exclusion, instrumental).
narrative_ontology:cs_reference_frame('e6844ac2-aca3-4ea4-bb65-d194ba8571dc', post_revolutionary_constitutional_settlement).
narrative_ontology:cs_drift_state('e6844ac2-aca3-4ea4-bb65-d194ba8571dc', consolidation_phase, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e6844ac2-aca3-4ea4-bb65-d194ba8571dc', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_democratic_parties).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, civilian_government_institutions).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, liberal_civil_society).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, jamaat_e_islami_political_wing).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, military_autonomous_authority).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, islamist_civil_society_organizations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Electoral beneficiaries of the secular mandate; they dominate parliament and cabinet formation. The Charter's exclusion of Islamist competitors secures their political space. They can exit by losing elections (mobile), but the constraint structure makes Islamist return unlikely. They also set the agenda for constitutional interpretation through their legislative majority.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_democratic_parties, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_democratic_parties, agenda_setter).

% Cabinet, ministries, parliament — formally supreme over military per Charter. They administer the exclusion of Islamist parties (party bans, electoral vetting) and oversee military subordination (budget control, appointment authority). Their exit is constrained: they are the constraint's administrators; leaving means state collapse. They benefit from the institutional authority the constraint grants.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, civilian_government_institutions, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__secular_democratic_reading, civilian_government_institutions, beneficiary).

% NGOs, rights groups, independent media, professional associations. They gain legal protections, funding access, and operating space under the secular democratic order. They can exit by emigrating or going underground (mobile), but the constraint subsidizes their activity. They are not the primary agenda-setters but are key coalition members.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, liberal_civil_society, beneficiary,
    organized, biographical, mobile, national).

% The largest Islamist political organization, historically dominant in opposition. Under this reading's Charter implementation, they face party ban, leadership imprisonment, electoral exclusion, and asset seizure. Their organizational structure persists underground and in exile. Exit options are constrained: they cannot legally participate, but maintain transnational networks and grassroots support. They are identity_locked in the sense that their organizational identity fuses religious-political mission with the specific historical struggle — but 'constrained' better captures their retained organizational capacity without legal pathway.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, jamaat_e_islami_political_wing, payer,
    organized, generational, constrained, national).

% The military's institutional autonomy: independent budget, officer promotion control, doctrinal independence, internal justice system, economic enterprises. The Charter subordinates all this to civilian defense ministry and parliamentary oversight. The military's self-concept as 'guardian of the nation' is fused with autonomous authority — losing it is experienced as institutional identity loss, not just resource loss. Exit is identity_locked: the institution cannot 'leave' the subordination without ceasing to be the military as it understands itself; coup is the only exit, which the constraint's enforcement machinery is designed to prevent.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, military_autonomous_authority, payer,
    institutional, generational, identity_locked, national).

% Charities, schools, media, professional networks affiliated with Islamist currents. They face registration denials, funding freezes, surveillance, and leadership harassment. They can operate semi-legally or underground (constrained), but cannot access state resources or legal protections. Their organizational identity is less fused than Jamaat-e-Islami's political wing — some fragment, some adapt, some persist in exile.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, islamist_civil_society_organizations, payer,
    organized, biographical, constrained, national).

% Academic observers, UN/regional human rights bodies, democracy indices. They document the Charter's implementation, measure democratic quality, track exclusions. They do not collect from or pay into the constraint; their role is analytical witnessing. Their exit is analytical — they can change their assessment without material consequence.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, constitutional_scholars_international_monitors, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the founding problem of military dominance and Islamist political capture by establishing a single legitimate ordering: civilian democratic authority, secular state principle, military subordination. Replaces the previous dual-sovereignty (military + Islamist street power) with a unified civilian democratic chain of command.
% TRANSFER_FUNCTION: Transfers political access and state resources from Islamist actors (Jamaat-e-Islami, allied civil society) and military autonomous authority to secular democratic parties and civilian institutions. Moves institutional autonomy from military to civilian control; moves electoral competition from open contest to curated secular arena.
% ABSENT_VOICES: Rural conservative constituencies who supported Islamist parties for service delivery not ideology; junior officers who might prefer professional military over political guardian role; diaspora Islamist networks who fund domestic resistance but have no domestic political voice; victims of Islamist violence in prior period who may support exclusion but are not consulted on democratic design.
% DISAPPEARANCE_RATIONALE: If the secular democratic mandate vanished overnight: Jamaat-e-Islami would re-enter legal politics immediately; military would reassert autonomous authority within weeks; civilian government would lose its constitutional shield against both; the post-revolutionary political settlement would collapse into three-way contestation (secular democrats, Islamists, military). The world rearranges violently.
% FOUNDING_PROBLEM: The 2010s-2020s period featured dual sovereignty: military as autonomous guardian institution (controlling defense, foreign policy, economic enterprises) and Islamist street mobilization (Jamaat-e-Islami) as de facto veto on secular policy. The July Charter was drafted to resolve this by establishing civilian democratic supremacy, secularism, and military subordination as a unified package.
% FOUNDING_PROBLEM_CORROBORATION: Secular democratic parties and civilian institutions attest the founding problem persists (military would return, Islamists would capture democracy) — self-interested testimony. Jamaat-e-Islami and military factions attest the problem is solved or was exaggerated — also self-interested. Independent corroboration: international democracy indices (V-Dem, Freedom House) show military influence declined but not eliminated; Islamist parties remain banned but grassroots support persists per independent polling; constitutional scholars outside the benefiting coalition (e.g., comparative constitutional law experts not affiliated with the governing coalition) describe the founding problem as 'partially resolved but structurally unstable' — the military subordination is real but contested; the Islamist exclusion is effective but generates legitimacy deficit.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__secular_democratic_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__secular_democratic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__secular_democratic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__secular_democratic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.72) reflects substantial exclusionary force: Political Islam actors are structurally barred from the political arena the Charter organizes, and military autonomy is actively constrained. Suppression (0.78) is high because maintaining this exclusion and subordination requires active enforcement — party bans, electoral engineering, military purge/retirement programs, surveillance of Islamist networks. Theater ratio (0.28) is moderate: the secular democratic coordination function (elections, civilian oversight mechanisms) is real and functional, but a growing share of enforcement activity serves the exclusionary function rather than democratic consolidation. Accessibility collapse (0.42) is moderate — alternatives (Islamist participation, military autonomy) are not fully collapsed; they persist underground and in exile. Resistance (0.68) is high from both victim groups. The measurement series show extraction and suppression rising over the interval as the secular democratic coalition consolidates power and the military's autonomous authority is systematically dismantled, while theater rises as democratic rituals are performed increasingly for legitimacy rather than function.
 *
 * PERSPECTIVAL GAP:
 *   From the secular democratic parties' seat (beneficiary/agenda_setter), the constraint is genuine coordination solving the founding problem of military dominance and Islamist political capture. From Jamaat-e-Islami's seat (victim), it is exclusionary extraction masquerading as democratic principle. From the military's seat (victim/former agenda_setter), it is forced subordination extracting institutional autonomy. The engine computes these per-seat types from the structural data; the claimed_type (tangled_rope) reflects the structural reality that BOTH coordination and extraction are genuinely present.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: secular_democratic_parties (collect political power and institutional protection), civilian_government_institutions (gain formal supremacy), liberal_civil_society (gain rights framework). Victims: jamaat_e_islami_political_wing (excluded from politics, leadership imprisoned/exiled), military_autonomous_authority (loses autonomous budget, promotion control, doctrinal independence), islamist_civil_society_organizations (restricted, surveilled). Directionality derives from these declarations: beneficiaries have low d (subsidized by constraint), victims have high d (extracted from). Military autonomous authority is identity_locked — its institutional self-concept is fused with guardian role, making exit from the constraint structurally painful. Jamaat-e-Islami is constrained — they have organizational capacity but no legal political pathway.
 *
 * MANDATROPHY ANALYSIS:
 *   The Charter was founded to solve military dominance and Islamist capture of the 2010s-2020s period. That founding problem is contested: secular democrats argue it persists (military would return, Islamists would capture democracy); Islamists and military factions argue the problem is solved or was exaggerated. The constraint persists partly because the secular democratic coalition benefits from the exclusionary structure (mandatrophy risk), but partly because genuine coordination (civilian supremacy, democratic elections) remains unstable without it. The classification as tangled_rope (not snare) captures this duality: the coordination function is real and the extraction is asymmetric — secular democrats coordinate, Islamists and military autonomy pay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Is this constraint a reading of the july_charter_sovereign_legitimacy kernel rather than an independent constraint?',
    'Comparative analysis of sibling readings'' structural parameters (beneficiaries, victims, extraction metrics) to confirm they share a referent but instantiate different constraints.',
    'If confirmed as a kernel reading, this constraint''s ε and structural data must be interpreted as reading-indexed over a fixed referent; sibling readings would generate separate constraint stories with distinct ε values.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Commitment that this is one reading of a contested kernel, not a standalone constraint.').

omega_variable(
    secular_vs_islamist_naturalness,
    'Does the secular democratic mandate reflect a genuine coordination function (stabilizing pluralistic politics) or does it function as cover for excluding Political Islam actors from legitimate participation?',
    'Longitudinal observation of whether excluded groups'' integration demands are addressed through constitutional amendment processes or permanently suppressed; cross-national comparison with transitional charters that included Islamist parties.',
    'If cover, classification shifts toward snare (extraction via exclusion); if genuine coordination, tangled_rope holds with secular democratic parties as coordinated beneficiaries and military as coordinated subordinate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_vs_islamist_naturalness, empirical, 'Whether the secular mandate''s coordination story is genuine or pretextual.').

omega_variable(
    military_subordination_durability,
    'Will military subordination to civilian authority persist as a stable equilibrium or revert to autonomous authority under stress?',
    'Track civil-military relations through crisis episodes (economic collapse, external threat, mass protest) to observe whether the military accepts civilian command or reasserts autonomy.',
    'If subordination collapses under stress, the constraint''s coordination function is fragile and extraction from military autonomous authority is temporary; if durable, tangled_rope stabilizes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(military_subordination_durability, empirical, 'Durability of the military subordination coordination function under crisis conditions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__secular_democratic_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(july_tr_t6, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 6, 0.16).
narrative_ontology:measurement(july_tr_t12, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement(july_tr_t18, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 18, 0.24).
narrative_ontology:measurement(july_tr_t24, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(july_tr_t30, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(july_be_t6, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(july_be_t12, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(july_be_t18, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 18, 0.63).
narrative_ontology:measurement(july_be_t24, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(july_be_t30, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 30, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(july_su_t6, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(july_su_t12, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(july_su_t18, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 18, 0.7).
narrative_ontology:measurement(july_su_t24, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 24, 0.74).
narrative_ontology:measurement(july_su_t30, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 30, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__secular_democratic_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.1).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy__guided_nationalism_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy__military_custodian_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, post_revolutionary_electoral_law).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, civil_military_relations_framework).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, political_party_registration_regime).

% DUAL FORMULATION NOTE:
% This constraint and its two sibling readings form the july_charter_sovereign_legitimacy constraint family. Each reading instantiates a different constraint from the same kernel: secular_democratic_reading (this story) has ε=0.72 with secular democrats as beneficiaries and Islamists/military autonomy as victims; guided_nationalism_reading would have Islamist parties as beneficiaries and secular liberals as victims; military_custodian_reading would have military as beneficiary/agenda_setter and civilian politicians as victims. The ε-invariance principle requires separate stories because ε differs by reading — the secular reading's extraction is the exclusion of Islamists, the guided reading's extraction would be the exclusion of secular liberals, the custodian reading's extraction would be the subordination of civilian authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(july_charter_sovereign_legitimacy__secular_democratic_reading, institutional, 0.15).
constraint_indexing:directionality_override(july_charter_sovereign_legitimacy__secular_democratic_reading, organized, 0.85).
constraint_indexing:directionality_override(july_charter_sovereign_legitimacy__secular_democratic_reading, institutional, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
