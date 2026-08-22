% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__hybrid_atrophy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__hybrid_atrophy_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: catastrophe_memory_preservation__hybrid_atrophy_reading
 *   human_readable: Catastrophe-Commemoration Ritual Cycle — Hybrid Atrophy Reading
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   A post-catastrophe diaspora community maintains an annual commemorative
 *   cycle — fast days, memorial liturgies, night vigils, remembrance
 *   assemblies, and children's preparation classes — instituted by the
 *   survivor generation to carry survival-competence to descendants: scarcity
 *   discipline, threat recognition, mutual-aid muster, and the practical
 *   knowledge of whom to trust and when to move. Under modernity the dangers
 *   those disciplines answered receded or changed form; the operational
 *   instruction stopped being taught and survives only as recited text, while
 *   the full costly form of the cycle persists year over year. This file
 *   instantiates the hybrid_atrophy_reading of the kernel
 *   catastrophe_memory_preservation: the arrangement once did real protective
 *   work, and what remains is a degenerate residue — maintained by inertia,
 *   identity fusion, and fidelity to the dead — delivering identity
 *   continuity that no longer repays its costs for the generation that bears
 *   them. Epsilon's referent is the standing arrangement, the commemorative
 *   cycle as it operates today, assessed by this reading's own lights; the
 *   sibling readings (survival_competence_reading, mourning_practice_reading)
 *   assess the same referent from different premises and are separate stories
 *   linked through network.affects_constraints. Claimed type and metrics are
 *   authored independently: the claim is piton; the metrics describe what the
 *   record shows of the cycle's operation.
 *
 * KEY AGENTS:
 *   - - ritual_leadership: agenda-setter (organized/identity_locked) — administers the cycle, could amend or retire elements, bears the legitimacy cost of amendment
 *   - - elder_memory_keepers: principal beneficiary (moderate/identity_locked) — receives identity and grief goods; the strongest maintenance constituency, and a dying one
 *   - - present_generation_practitioners: primary payer (moderate/constrained) — bears time, money, and obligation costs without the rites' former practical payoff
 *   - - nonobservant_in_group_descendants: excluded voice with residual payer exposure (moderate/mobile) — prefers civic-scale commemoration, unrepresented in liturgical governance
 *   - - ritual_studies_scholars: analytical observer (analytical/analytical) — documents the atrophy of operational content from outside the community's decision-making
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.42).
domain_priors:suppression_score(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.3).
domain_priors:theater_ratio(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__hybrid_atrophy_reading, piton).
narrative_ontology:human_readable(catastrophe_memory_preservation__hybrid_atrophy_reading, "Catastrophe-Commemoration Ritual Cycle — Hybrid Atrophy Reading").
narrative_ontology:topic_domain(catastrophe_memory_preservation__hybrid_atrophy_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__hybrid_atrophy_reading, '8cc57f8f-0b3f-481e-999b-0eda36e302ee').
narrative_ontology:cs_kernel_codification('8cc57f8f-0b3f-481e-999b-0eda36e302ee', fixed_text).
narrative_ontology:cs_authority_grounding('8cc57f8f-0b3f-481e-999b-0eda36e302ee', lineage).
narrative_ontology:cs_interpretation_layer_present('8cc57f8f-0b3f-481e-999b-0eda36e302ee').
narrative_ontology:cs_reading_relation('8cc57f8f-0b3f-481e-999b-0eda36e302ee', catastrophe_memory_preservation__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('8cc57f8f-0b3f-481e-999b-0eda36e302ee', catastrophe_memory_preservation__mourning_practice_reading, coexists_with).
narrative_ontology:cs_axiom('8cc57f8f-0b3f-481e-999b-0eda36e302ee', foundational, ritual_operational_content_has_atrophied).
narrative_ontology:cs_axiom_status(ritual_operational_content_has_atrophied, holdable).
narrative_ontology:cs_axiom_grounding('8cc57f8f-0b3f-481e-999b-0eda36e302ee', ritual_operational_content_has_atrophied, empirically_contingent).
narrative_ontology:cs_axiom('8cc57f8f-0b3f-481e-999b-0eda36e302ee', secondary, identity_benefit_insufficient_to_restore_function).
narrative_ontology:cs_axiom_status(identity_benefit_insufficient_to_restore_function, holdable).
narrative_ontology:cs_axiom_grounding('8cc57f8f-0b3f-481e-999b-0eda36e302ee', identity_benefit_insufficient_to_restore_function, instrumental).
narrative_ontology:cs_reference_frame('8cc57f8f-0b3f-481e-999b-0eda36e302ee', survival_competence_transmission_frame).
narrative_ontology:cs_drift_state('8cc57f8f-0b3f-481e-999b-0eda36e302ee', late_modernity, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8cc57f8f-0b3f-481e-999b-0eda36e302ee', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__hybrid_atrophy_reading, elder_memory_keepers).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__hybrid_atrophy_reading, commemorating_community_members).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_practitioners).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__hybrid_atrophy_reading, nonobservant_in_group_descendants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_practitioners).
narrative_ontology:constraint_vindicates(catastrophe_memory_preservation__hybrid_atrophy_reading, intergenerational_memory_obligation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Officiates and schedules the annual commemorative cycle: fast days, memorial liturgies, night vigils, remembrance assemblies, and the classes that prepare children for them. Administers the memorial endowment and decides which elements of the inherited rite may be shortened, merged, or dropped. Proposals to simplify carry a legitimacy price — congregants read amendment as disrespect to the founding generation — so the full form is preserved year over year. Receives honorific standing and modest stipends; vocation, ordination, and often family lineage are bound up with keeping the cycle running. Leaving the role would mean leaving the community that constitutes their life.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, ritual_leadership, agenda_setter,
    organized, generational, identity_locked, regional).

% Aging members with direct or near-direct family memory of the catastrophe. For them the yearly rites deliver real goods: contact with the dead, continuity with parents and grandparents, a public frame for grief. They staff the volunteer committees, fund the memorial events, and speak most forcefully against simplification. They will not see many more cycles; the practice is set to outlast the people who currently find it most nourishing.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, elder_memory_keepers, beneficiary,
    moderate, biographical, identity_locked, regional).

% Adult members in their working and child-raising decades. They fast, attend long liturgies after work, travel to commemorative gatherings, pay dues that maintain memorial buildings, and shepherd children through preparation classes. What they receive back is belonging and a story of where they come from; what the rites no longer deliver is anything they could use — the disciplines rehearsed answer dangers that no longer threaten them in that form. Scaling down draws reproach from parents and elders; stepping away entirely costs family harmony and their social world.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_practitioners, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_practitioners, beneficiary).

% Descendants who have quietly dropped most observance — they attend the main anniversary service, skip the rest, eat as they please. They stay inside the family and community orbit, where the expectation is still felt: phone calls before fast days, a seat expected at the memorial table, a low-grade accounting of who remembered properly. They would prefer a shorter civic-style commemoration and say so at home, but no one has offered them a seat where the liturgy's shape is decided.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, nonobservant_in_group_descendants, excluded,
    moderate, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_preservation__hybrid_atrophy_reading, nonobservant_in_group_descendants, payer).

% Academic historians and liturgists who study the rite's development. Their monographs trace which portions once encoded practical instruction — scarcity rationing tables inside fast-day rubrics, watch-rotation language in night vigils, asset-transfer formulas in memorial bequests — and document when each stopped being taught as instruction and came to be recited as text. They publish, lecture, and advise documentary projects; they hold no place in the committees that decide the rite's future form.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, ritual_studies_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_preservation__hybrid_atrophy_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_preservation__hybrid_atrophy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes communal remembrance on a fixed calendar, gathers geographically dispersed members around shared dates, and transmits the catastrophe narrative to children. Historically it also rehearsed survival routines — scarcity discipline, threat recognition, mutual-aid muster; today it coordinates memorial assembly and identity reaffirmation.
% TRANSFER_FUNCTION: Moves time, labor, and money from present-generation practitioners into the commemorative apparatus — liturgy upkeep, memorial events, buildings, schools — and into the community's symbolic account of continuity. Historically it moved survival-relevant knowledge and readiness across the generational gap.
% ABSENT_VOICES: Nonobservant descendants and secularized members, who prefer abbreviated or civic commemoration, are not seated in liturgical governance. The commemorated dead cannot consent to the forms their memory takes. Scholars who document the atrophy of the operational content rarely enter the committee rooms where the rite's future is decided.
% DISAPPEARANCE_RATIONALE: If the cycle vanished overnight, the community's calendar, its largest gatherings, its philanthropic channels, and its principal identity markers would all rearrange; grief would need new containers; the memorial institutions the cycle funds would contract; families would lose the annual occasion that gathers their scattered branches.
% FOUNDING_PROBLEM: After the catastrophe, the community needed to preserve survival-competence in descendants who had not lived through it — threat recognition, scarcity discipline, mutual-aid mobilization, and the practical judgment of when to move and whom to trust. The ritual cycle was built to encode and rehearse that knowledge until it was needed again.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: academic liturgical historians trace the operational content (rationing tables, watch rotations, asset-transfer rubrics) and document its disappearance from instruction; practitioner surveys show even highly observant members describe the rites as memorial rather than preparatory; no contemporary communal authority claims the rites confer practical survival skill. The dead-problem finding rests on sources with no stake in the ritual economy.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__hybrid_atrophy_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__hybrid_atrophy_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__hybrid_atrophy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__hybrid_atrophy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__hybrid_atrophy_reading_tests).
:- end_tests(catastrophe_memory_preservation__hybrid_atrophy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.42 — moderate and declining: the cycle still imposes real costs (multi-hour liturgies, fast observance, travel, dues, preparation labor) on people who receive no practical return, but the demands have lightened across the interval and enforcement has softened. Suppression is 0.30: the era of expulsion-grade communal sanction is gone; what remains is reproach, family pressure, and internalized filial obligation. Suppression is authored as a raw structural property and is deliberately unscaled — the engine scales only extractiveness, via directionality and scope. Theater ratio is 0.71: the majority of cycle activity is performative commemoration — recitation, assembly, catering, programming — with the instructional payload gone; a shrinking minority (crisis-time mutual-aid activation, charitable channels) still does work. Accessibility collapse is 0.30: alternatives plainly exist and are used — civic memorials, private remembrance, reform liturgies, plain nonobservance — so understanding the arrangement does not close exits. Resistance is 0.50: youth disengagement, intermarriage-driven attrition, and recurring simplification proposals meet every cycle. The three temporal series share one grid, with every metric authored at every examined point; the suppression series is included because this story specifically tracks enforcement-capacity decay — the sanction machinery that once backed the cycle has visibly wound down. No cyclical modeling: the annual rhythm recurs, but the multi-decade trajectory is monotonic atrophy, not oscillation.
 *
 * PERSPECTIVAL GAP:
 *   Seats should classify differently. From the elder keepers' seat the cycle is a living good — costly, worth it, closer to rope. From the present practitioners' seat it is uncompensated cost — closer to extraction. From the leadership seat it is near-symmetric: the standing received roughly balances the labor borne, and identity lock makes exit unthinkable rather than merely expensive. The engine computes these divergences from the structural data; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (elder_memory_keepers, and incidentally the commemorating membership) sit near the subsidized end; declared victims (present_generation_practitioners, nonobservant_in_group_descendants) sit near the target end, with the practitioners' constrained exit pushing them further toward full-target than the mobile nonobservants. The leadership seat declares neither role: derivation places it near symmetric, which is the substantive point — the administrators are not capturers. Receipt of gains is affirmatively diffuse: checked seat by seat, no one collects the cycle's costs as rent. Leadership stipends are coordination overhead, not capture, and the bulk of what the costs purchase is performance and symbolic continuity that dissipates across the whole membership rather than pooling in any seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — carrying survival-competence across a generational gap — is dead by this reading's lights, corroborated from outside the beneficiary set; yet the arrangement persists and the world would visibly rearrange without it. That dead-problem-by-world_rearranges mismatch is the zombie signature, and it cross-checks cleanly against the computed path: theater 0.71, diffuse receipt, prohibitive fixing cost — the piton profile, not a capture profile. The classification earns its keep by blocking two mislabels. As snare: there is no capturer — gain_flow is affirmatively diffuse, and the most interested seat, the elders, is passing away rather than consolidating. As rope: the coordination function that would justify the costs has atrophied — the cycle no longer solves a collective-action problem so much as dramatize one that closed. It equally blocks scaffold: the tradition never built a sunset clause, and that absence is itself diagnostic — an arrangement designed as transitional would have specified its own retirement. Mandatrophy is resolved here in substance: the mandate outlived its function, and what remains is administered inheritance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the kernel catastrophe_memory_preservation (reading: hybrid_atrophy_reading). Which structural facts would flip under the sibling readings?',
    'Cross-reading corpus comparison: compile the three reading-stories and diff their epsilon, beneficiary/victim sets, and computed types. The disagreement localizes to whether operational content persists (versus survival_competence_reading) and whether present costs are adequately repaid by identity goods (versus mourning_practice_reading).',
    'Under survival_competence_reading, epsilon drops toward coordination-cost levels and the type moves toward rope; under mourning_practice_reading, the costs reframe as the intrinsic price of identity goods and extraction approaches the identity_coordination floor. This story''s piton verdict holds only under the atrophy premise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification: sibling readings of the same kernel yield different types from the same referent.').

omega_variable(
    residual_operational_content,
    'Does any operational content still transfer — do fast-trained members actually mobilize faster in scarcity events, do vigil networks still activate for real threats?',
    'Disaster-response and mutual-aid studies comparing observant and non-observant cohorts in crises (blackouts, floods, community emergencies); liturgical audits establishing whether instructional rubrics are still taught as instruction anywhere in the community.',
    'If measurable transfer persists, the survival component is partly live and the type shifts toward tangled_rope; if none, the atrophy premise hardens and the piton verdict stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_operational_content, empirical, 'Whether the rites retain any of their original practical payload.').

omega_variable(
    identity_benefit_sufficiency,
    'Do the identity and grief goods the cycle delivers actually repay present-generation costs once those costs are honestly counted?',
    'Weighted wellbeing and belonging surveys across observance strata, priced against measured time, dietary, travel, and dues burdens; revealed-preference checks distinguishing voluntary payment from payment under family pressure.',
    'If benefits repay costs for most bearers, the arrangement is costly-but-valued coordination (rope-flavored) and the atrophy framing overstates degradation; if not, the uncompensated-residue reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_benefit_sufficiency, preference, 'Whether identity goods compensate the costs this reading counts as uncompensated.').

omega_variable(
    internalized_vs_structural_obligation,
    'Is the participation that remains sustained by internalized obligation (guilt, filial duty that would persist without sanction) or by structural communal pressure?',
    'Post-exit trajectory of disaffiliated members: whether private observance resumes, whether relief or loss dominates their accounts, whether reproach follows them; compare cohorts that left under sanction-heavy versus sanction-light communal regimes.',
    'If internalized, the scalar suppression understates the lock — carriers take the obligation with them after exit and the arrangement outlives its own enforcement machinery; if structural, the falling suppression series captures the true decay.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_obligation, empirical, 'Internalized versus structural mechanism behind residual participation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__hybrid_atrophy_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(cata_tr_t10, observed).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement_basis(cata_tr_t20, observed).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 30, 0.43).
narrative_ontology:measurement_basis(cata_tr_t30, observed).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 40, 0.5).
narrative_ontology:measurement_basis(cata_tr_t40, observed).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 50, 0.56).
narrative_ontology:measurement_basis(cata_tr_t50, observed).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 60, 0.62).
narrative_ontology:measurement_basis(cata_tr_t60, observed).
narrative_ontology:measurement(cata_tr_t70, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 70, 0.67).
narrative_ontology:measurement_basis(cata_tr_t70, observed).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 80, 0.71).
narrative_ontology:measurement_basis(cata_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(cata_be_t10, observed).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement_basis(cata_be_t20, observed).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 30, 0.49).
narrative_ontology:measurement_basis(cata_be_t30, observed).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 40, 0.47).
narrative_ontology:measurement_basis(cata_be_t40, observed).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 50, 0.45).
narrative_ontology:measurement_basis(cata_be_t50, observed).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 60, 0.44).
narrative_ontology:measurement_basis(cata_be_t60, observed).
narrative_ontology:measurement(cata_be_t70, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 70, 0.43).
narrative_ontology:measurement_basis(cata_be_t70, observed).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 80, 0.42).
narrative_ontology:measurement_basis(cata_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 10, 0.57).
narrative_ontology:measurement_basis(cata_su_t10, observed).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement_basis(cata_su_t20, observed).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 30, 0.46).
narrative_ontology:measurement_basis(cata_su_t30, observed).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 40, 0.41).
narrative_ontology:measurement_basis(cata_su_t40, observed).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 50, 0.37).
narrative_ontology:measurement_basis(cata_su_t50, observed).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 60, 0.34).
narrative_ontology:measurement_basis(cata_su_t60, observed).
narrative_ontology:measurement(cata_su_t70, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 70, 0.32).
narrative_ontology:measurement_basis(cata_su_t70, observed).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 80, 0.3).
narrative_ontology:measurement_basis(cata_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__hybrid_atrophy_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation__mourning_practice_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the community's memory ritual' decomposes, per epsilon-invariance, into three structurally distinct claims sharing one referent (the standing commemorative cycle) and one kernel. survival_competence_reading is upstream: its historical premise (the rites once carried operational content) is the shared foundation every reading cites, and it is the claim this reading accepts for the past while denying for the present. mourning_practice_reading is the contemporaneous rival valuation: it accepts the absence of operational content but denies that this constitutes degradation. This reading's epsilon (0.42, moderate, declining) differs from both siblings' by construction — same referent, different premises. Each story links the others through affects_constraints; orphan stories would break contamination-propagation analysis across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
