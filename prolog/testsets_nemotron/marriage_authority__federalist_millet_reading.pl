% ============================================================================
% CONSTRAINT STORY: marriage_authority__federalist_millet_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__federalist_millet_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: marriage_authority__federalist_millet_reading
 *   human_readable: Federalist-Millet Marriage Authority Fragmentation
 *   domain: legal_pluralism/constitutional_law/comparative_family_law
 *
 * SUMMARY:
 *   This constraint describes the post-colonial constitutional settlement
 *   that fragments marriage authority across religious communities (millet
 *   system) as a deliberate anti-majoritarian mechanism. The arrangement
 *   prevents a unified family code that would reflect majoritarian cultural
 *   norms, instead vesting personal law authority in community-specific
 *   bodies. The state enforces but does not author family law norms, creating
 *   a coordination structure where legal pluralism functions as a
 *   consociational stability device. Legislative paralysis on Uniform Civil
 *   Code reform is a feature — it preserves the elite bargain that maintains
 *   communal peace. Extraction is low because the arrangement primarily
 *   coordinates conflict avoidance; the theater ratio captures the growing
 *   performative maintenance of the 'secularism' framing while the
 *   substantive pluralism persists.
 *
 * KEY AGENTS:
 *   - minority_communities: Primary beneficiary (institutional/moderate) — hold recognized personal law authority
 *   - state_elite_bargain_participants: Secondary beneficiary (institutional/biographical) — political actors who maintain the consociational settlement
 *   - women_in_minority_communities: Payer (organized/constrained) — bear gender-differentiated costs within community personal laws
 *   - secularist_legislators: Excluded (powerful/constrained) — would impose unified code but blocked by elite bargain
 *   - supreme_court_justices: Observer (institutional/generational) — navigate constitutional equality vs. pluralism tension through case-by-case review
 *   - majority_community_leaders: Agenda_setter (institutional/biographical) — hold de facto veto over UCC legislation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__federalist_millet_reading, 0.18).
domain_priors:suppression_score(marriage_authority__federalist_millet_reading, 0.12).
domain_priors:theater_ratio(marriage_authority__federalist_millet_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__federalist_millet_reading, rope).
narrative_ontology:human_readable(marriage_authority__federalist_millet_reading, "Federalist-Millet Marriage Authority Fragmentation").
narrative_ontology:topic_domain(marriage_authority__federalist_millet_reading, "legal_pluralism/constitutional_law/comparative_family_law").

domain_priors:requires_active_enforcement(marriage_authority__federalist_millet_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__federalist_millet_reading, '900b2433-5d87-4581-83c9-dd0d812d4bbe').
narrative_ontology:cs_kernel_codification('900b2433-5d87-4581-83c9-dd0d812d4bbe', formalized).
narrative_ontology:cs_authority_grounding('900b2433-5d87-4581-83c9-dd0d812d4bbe', lineage).
narrative_ontology:cs_interpretation_layer_present('900b2433-5d87-4581-83c9-dd0d812d4bbe').
narrative_ontology:cs_reading_relation('900b2433-5d87-4581-83c9-dd0d812d4bbe', marriage_authority__communal_autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('900b2433-5d87-4581-83c9-dd0d812d4bbe', marriage_authority__gender_rights_reading, influences).
narrative_ontology:cs_reading_relation('900b2433-5d87-4581-83c9-dd0d812d4bbe', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_reading_relation('900b2433-5d87-4581-83c9-dd0d812d4bbe', marriage_authority__secularist_reading, coexists_with).
narrative_ontology:cs_axiom('900b2433-5d87-4581-83c9-dd0d812d4bbe', foundational, pluralism_as_anti_majoritarian_shield).
narrative_ontology:cs_axiom_status(pluralism_as_anti_majoritarian_shield, holdable).
narrative_ontology:cs_axiom_grounding('900b2433-5d87-4581-83c9-dd0d812d4bbe', pluralism_as_anti_majoritarian_shield, conventional).
narrative_ontology:cs_axiom('900b2433-5d87-4581-83c9-dd0d812d4bbe', secondary, legislative_paralysis_as_stability_feature).
narrative_ontology:cs_axiom_status(legislative_paralysis_as_stability_feature, holdable).
narrative_ontology:cs_axiom_grounding('900b2433-5d87-4581-83c9-dd0d812d4bbe', legislative_paralysis_as_stability_feature, instrumental).
narrative_ontology:cs_reference_frame('900b2433-5d87-4581-83c9-dd0d812d4bbe', constituent_assembly_consociational_settlement).
narrative_ontology:cs_drift_state('900b2433-5d87-4581-83c9-dd0d812d4bbe', contemporary_judicial_activism_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('900b2433-5d87-4581-83c9-dd0d812d4bbe', '').
narrative_ontology:cs_kernel_id(marriage_authority__federalist_millet_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, minority_communities).
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, state_elite_bargain_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_authority__federalist_millet_reading, women_in_minority_communities).
narrative_ontology:constraint_vindicates(marriage_authority__federalist_millet_reading, consociational_anti_tyranny_principle).
narrative_ontology:constraint_vindicates(marriage_authority__federalist_millet_reading, federalist_legal_pluralism_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold recognized legal authority over marriage, divorce, inheritance within their community. This authority is constitutionally protected and insulated from majoritarian legislative override. Exit from this authority structure would mean losing communal legal autonomy — a core component of collective identity. They benefit from the arrangement's prevention of a unified family code that would reflect majority cultural norms.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, minority_communities, beneficiary,
    institutional, generational, identity_locked, national).

% Political actors (party leaders, coalition architects) who maintain the consociational settlement as a governing coalition requirement. They benefit from the stability the arrangement provides — communal peace enables their political survival. They administer the arrangement by blocking UCC legislation and defending personal law autonomy in legislative and executive fora. Their exit is constrained by coalition arithmetic: challenging the arrangement risks communal mobilization and government collapse.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, state_elite_bargain_participants, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__federalist_millet_reading, state_elite_bargain_participants, agenda_setter).

% Bear gender-differentiated costs under community personal laws: unequal divorce rights, maintenance, inheritance, guardianship. These laws are state-enforced through the personal law system. Exit is identity_locked — leaving the community's legal framework requires religious conversion or secular marriage under the Special Marriage Act, both of which carry severe social, familial, and economic costs. They are organized through feminist groups but their exit from the constraint's effects is structurally blocked by identity fusion.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, women_in_minority_communities, payer,
    organized, biographical, identity_locked, national).

% Elected representatives who advocate for a Uniform Civil Code as a democratic mandate. They are structurally excluded from reforming the arrangement because the consociational elite bargain gives minority community leaders veto power, and majority community leaders calculate that UCC legislation would cost more electoral support than it gains. Their exit from the constraint is constrained — they can propose legislation but cannot pass it without breaking the coalition arithmetic that sustains the arrangement.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, secularist_legislators, excluded,
    powerful, biographical, constrained, national).

% Navigate the tension between constitutional equality guarantees (Articles 14, 15, 21) and the personal law system's constitutional protection (Article 25, 26, 29). They cannot abolish personal law but incrementally impose a constitutional floor through case-by-case review (Shah Bano, Shayara Bano, etc.). They do not collect from or pay into the arrangement; their seat is analytical — they interpret the kernel's drift.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, supreme_court_justices, observer,
    institutional, generational, analytical, national).

% Hold de facto veto over UCC legislation through electoral coalition arithmetic. They could push for UCC but calculate that the political cost (loss of minority votes, communal polarization) exceeds the benefit. They are not direct beneficiaries of the personal law system (their community is largely governed by codified Hindu law), but they administer the arrangement by sustaining the legislative paralysis. Their exit is arbitrage-grade — they could pivot to UCC advocacy if electoral calculus shifted.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, majority_community_leaders, agenda_setter,
    institutional, biographical, arbitrage, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents majoritarian domination of family law in a religiously diverse polity by fragmenting marriage authority across communities. Solves the collective-action problem of constitutional stability: no single community can impose its family norms on others, and the state cannot impose a unified code without triggering communal conflict.
% TRANSFER_FUNCTION: Moves legislative authority over family law from a hypothetical unified democratic legislature to community-specific bodies. The transfer is authority (who decides), not primarily material resources. Minority communities gain protected legal autonomy; women in those communities bear gender-differentiated costs; the democratic majority loses legislative competence over a core social domain.
% ABSENT_VOICES: Women in minority communities who would reform personal laws from within are structurally excluded from community leadership bodies that interpret and administer personal law. Their voices are absent from the elite bargain that maintains the arrangement. LGBTQ+ persons in all communities are excluded from personal law recognition entirely. These absences are not accidental — the arrangement's stability depends on community leadership bodies that are male-dominated and heteronormative.
% DISAPPEARANCE_RATIONALE: If the fragmented authority arrangement vanished overnight, a unified family code would be legislated (likely Hindu-law-inflected), minority communities would lose protected legal autonomy, communal mobilization would erupt, and women's rights advocates would gain a single legislative target for gender-equal reform. The polity would reorganize around majoritarian family law with constitutional equality challenges replacing pluralism management.
% FOUNDING_PROBLEM: Post-colonial constitutional settlement in a religiously diverse polity with a recent history of partition: how to prevent a majoritarian state from imposing majority cultural norms on minority communities through family law, while maintaining a single sovereign legal order.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the constitutional text (Articles 25-30, 44), Constituent Assembly debates (Muslim members' insistence on personal law protection), and the continued refusal of successive governments to enact UCC despite Article 44 directive. Corroboration from outside the beneficiary set: feminist scholars (Flavia Agnes, Nivedita Menon) acknowledge the anti-majoritarian function while contesting its gender costs; political scientists (Arend Lijphart, Rochana Bajpai) analyze the consociational logic. The state's own Law Commission reports (2018) confirm the founding problem remains live but note the gender-equality tension.
narrative_ontology:disappearance_verdict(marriage_authority__federalist_millet_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__federalist_millet_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__federalist_millet_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(marriage_authority__federalist_millet_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__federalist_millet_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__federalist_millet_reading_tests).
:- end_tests(marriage_authority__federalist_millet_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed_type=rope reflects this reading's structural claim: the fragmented authority is a genuine coordination mechanism solving the majoritarian domination problem. The low extractiveness (0.18) captures that the primary function is conflict prevention, not rent extraction. Suppression (0.12) is low because exit to secular courts exists for some matters, and communities internally enforce their codes. Theater ratio (0.25) measures the gap between the 'secular state' self-presentation and the actual persistence of community-authored law. Accessibility_collapse (0.35) is moderate — alternatives (UCC, judicial harmonization) exist but are politically blocked. Resistance (0.45) reflects feminist and secularist challenges. The engine computes per-seat types from these structural inputs; the payer seat (women_in_minority_communities) will compute differently from the beneficiary seats.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (minority_communities, state_elite_bargain_participants) experience this as a protective coordination device — legal pluralism prevents majoritarian cultural imposition. The payer seat (women_in_minority_communities) experiences intra-community gender hierarchy codified and state-enforced. The excluded seat (secularist_legislators) experiences blocked democratic reform. The observer seat (supreme_court_justices) navigates the contradiction through incremental constitutional review. The engine derives these divergent effective extractions from the declared structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Minority_communities are beneficiaries (d ~ 0.2) — they hold recognized legal authority over family matters, insulated from majoritarian override. State_elite_bargain_participants are beneficiaries (d ~ 0.25) — the arrangement preserves their political coalition. Women_in_minority_communities are payers (d ~ 0.75) — they bear gender-differentiated costs under community personal laws with constrained exit (identity_locked via religious/communal identity). Secularist_legislators are excluded (d ~ 0.6) — they would reform but are blocked by the elite bargain. Supreme_court_justices are observers (d ~ 0.5) — analytical seat. Majority_community_leaders are agenda_setters (d ~ 0.3) — they hold veto power but do not directly collect extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing majoritarian domination of family law in a religiously diverse polity — remains live (founding_problem_status=live). The arrangement still performs its anti-tyranny function. However, the gender equality cost has accumulated without a corresponding adjustment mechanism, creating mandatrophy tension: the constraint's original anti-majoritarian justification now also shields intra-community gender hierarchy. The classification as rope (not tangled_rope) reflects this reading's position that the coordination function remains primary and the gender-equality costs are a separate contestation (gender_rights_reading), not an extraction built into this constraint's structure. If the gender-equality costs were attributed to this constraint, it would reclassify toward tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_frame_federalist_millet_reading,
    'This constraint is the federalist_millet_reading of the marriage_authority kernel. What structural elements would change under sibling readings?',
    'Compare beneficiary/victim structures, extractiveness values, and claimed_types across the five declared readings. This reading claims rope with minority_communities as beneficiary; gender_rights_reading would likely claim tangled_rope with women_in_minority_communities as victim; secularist_reading would claim snare or scaffold with democratic_majority as victim.',
    'Classification divergence across readings of the same kernel is the signal the committer frame exists to detect. If all readings produce the same type, the kernel framing may be obscuring structural sameness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_federalist_millet_reading, conceptual, 'Committer frame: this is one reading of a contested kernel; sibling readings instantiate different constraints').

omega_variable(
    coordination_extraction_boundary_gender_costs,
    'Are the gender-differentiated costs under community personal laws an extraction BY this constraint, or a separate constraint (gender_rights_reading) that overlaps the same kernel?',
    'Trace whether the elite bargain that maintains pluralism actively requires gender hierarchy as its price, or whether gender hierarchy is a pre-existing social structure that the pluralism arrangement merely fails to override. If the former, this constraint''s extractiveness is understated and it trends toward tangled_rope.',
    'If gender costs are intrinsic to the elite bargain, this reading''s claimed rope classification is a false summit — the arrangement extracts from women to purchase communal peace. If gender costs are exogenous, the rope classification holds and gender_rights_reading is the proper vehicle for that contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary_gender_costs, conceptual, 'Whether intra-community gender hierarchy is a price of the consociational bargain or a separate layer').

omega_variable(
    legislative_paralysis_as_feature_vs_bug,
    'Is the persistent failure to enact a Uniform Civil Code a stability feature of the consociational settlement (as this reading claims), or democratic failure that converts the arrangement into a piton/snare?',
    'Analyze whether political actors capable of passing UCC legislation choose not to (feature) or are structurally prevented by the arrangement''s own logic (bug). Track whether parliamentary majorities for UCC have existed and been blocked by coalition arithmetic.',
    'If paralysis is a chosen feature, the rope classification is stable. If paralysis is structural inability despite democratic mandate, the arrangement becomes a piton (degraded coordination maintained by inertia) or snare (extraction via blocked reform).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_paralysis_as_feature_vs_bug, empirical, 'Whether UCC legislative blockage is deliberate stability or democratic failure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__federalist_millet_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marriage_authority__federalist_millet_reading_tr_t1947, marriage_authority__federalist_millet_reading, theater_ratio, 1947, 0.1).
narrative_ontology:measurement(marriage_authority__federalist_millet_reading_tr_t1975, marriage_authority__federalist_millet_reading, theater_ratio, 1975, 0.18).
narrative_ontology:measurement(marriage_authority__federalist_millet_reading_tr_t1985, marriage_authority__federalist_millet_reading, theater_ratio, 1985, 0.22).
narrative_ontology:measurement(marriage_authority__federalist_millet_reading_tr_t2000, marriage_authority__federalist_millet_reading, theater_ratio, 2000, 0.24).
narrative_ontology:measurement(marriage_authority__federalist_millet_reading_tr_t2010, marriage_authority__federalist_millet_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(marriage_authority__federalist_millet_reading_tr_t2024, marriage_authority__federalist_millet_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(marriage_authority__federalist_millet_reading_be_t1947, marriage_authority__federalist_millet_reading, base_extractiveness, 1947, 0.12).
narrative_ontology:measurement(marriage_authority__federalist_millet_reading_be_t1975, marriage_authority__federalist_millet_reading, base_extractiveness, 1975, 0.15).
narrative_ontology:measurement(marriage_authority__federalist_millet_reading_be_t1985, marriage_authority__federalist_millet_reading, base_extractiveness, 1985, 0.17).
narrative_ontology:measurement(marriage_authority__federalist_millet_reading_be_t2000, marriage_authority__federalist_millet_reading, base_extractiveness, 2000, 0.18).
narrative_ontology:measurement(marriage_authority__federalist_millet_reading_be_t2010, marriage_authority__federalist_millet_reading, base_extractiveness, 2010, 0.18).
narrative_ontology:measurement(marriage_authority__federalist_millet_reading_be_t2024, marriage_authority__federalist_millet_reading, base_extractiveness, 2024, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(marriage_authority__federalist_millet_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__federalist_millet_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority__federalist_millet_reading, 0.08).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__judicial_harmonization_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__secularist_reading).

% DUAL FORMULATION NOTE:
% BGS-pattern decomposition of the marriage_authority kernel into five constraint stories. This reading (federalist_millet_reading) claims rope with minority_communities as beneficiary. Communal_autonomy_reading overlaps structurally but frames community religious tradition as the grounding. Gender_rights_reading claims tangled_rope with women_in_minority_communities as victim. Judicial_harmonization_reading claims scaffold with constitutional_floor as sunset target. Secularist_reading claims snare/scaffold with democratic_majority as victim. All linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority__federalist_millet_reading, organized, 0.75).
constraint_indexing:directionality_override(marriage_authority__federalist_millet_reading, institutional, 0.2).
constraint_indexing:directionality_override(marriage_authority__federalist_millet_reading, powerful, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
