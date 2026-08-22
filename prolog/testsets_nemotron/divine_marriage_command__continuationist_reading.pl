% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__continuationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__continuationist_reading, []).

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
 *   constraint_id: divine_marriage_command__continuationist_reading
 *   human_readable: Plural Marriage Doctrinal Continuity (Continuationist Reading)
 *   domain: religious/political/theological
 *
 * SUMMARY:
 *   The continuationist reading of the divine marriage command holds that
 *   plural marriage remains an eternal divine commandment suspended only
 *   prudentially by the 1890 Manifesto (and 1904 Second Manifesto) under
 *   federal duress — not doctrinally rescinded. This reading structures the
 *   theological field: mainstream institutional leadership maintains the
 *   suspension while preserving the underlying doctrine; fundamentalist
 *   splinter groups claim to practice the 'true' uncompromised commandment;
 *   federal authorities treat the suspension as regulatory compliance. The
 *   constraint operates as a tangled rope: it coordinates institutional
 *   survival (benefiting leadership and the federal state) while extracting
 *   from plural marriage practitioners who remain theologically legitimate
 *   but legally suppressed, and from women in fundamentalist communities who
 *   bear disproportionate costs. The claimed_type 'tangled_rope' reflects the
 *   dual coordination-extraction structure; the metrics describe the lived
 *   operation where doctrinal immunity functions as extraction cover.
 *
 * KEY AGENTS:
 *   - mainstream_institutional_leadership: Primary agenda_setter (institutional/arbitrage) — administers the suspension, controls doctrinal interpretation, benefits from federal accommodation
 *   - fundamentalist_splinter_groups: Secondary beneficiary (organized/identity_locked) — claim doctrinal continuity, extract compliance from members, serve as pressure valve for mainstream
 *   - federal_regulatory_state: Tertiary beneficiary (institutional/arbitrage) — achieves de facto monogamy enforcement without resolving theological claim
 *   - plural_marriage_practitioners: Primary victim (powerless/identity_locked) — theologically legitimate but legally criminalized, exit requires doctrinal apostasy
 *   - excluded_women_in_fundamentalist_communities: Secondary victim (powerless/trapped) — bear gendered costs of plural marriage with no voice in doctrinal interpretation
 *   - dissident_theologians: Excluded (moderate/constrained) — argue for doctrinal revision but are disciplined by institutional authority
 *   - analytical_observer: Observer (analytical/analytical) — sees full structure without stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, 0.68).
domain_priors:suppression_score(divine_marriage_command__continuationist_reading, 0.72).
domain_priors:theater_ratio(divine_marriage_command__continuationist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__continuationist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__continuationist_reading, "Plural Marriage Doctrinal Continuity (Continuationist Reading)").
narrative_ontology:topic_domain(divine_marriage_command__continuationist_reading, "religious/political/theological").

domain_priors:requires_active_enforcement(divine_marriage_command__continuationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__continuationist_reading, '605947d9-15fd-4a41-9553-0b808706386b').
narrative_ontology:cs_kernel_codification('605947d9-15fd-4a41-9553-0b808706386b', formalized).
narrative_ontology:cs_authority_grounding('605947d9-15fd-4a41-9553-0b808706386b', lineage).
narrative_ontology:cs_interpretation_layer_present('605947d9-15fd-4a41-9553-0b808706386b').
narrative_ontology:cs_reading_relation('605947d9-15fd-4a41-9553-0b808706386b', divine_marriage_command__substitutionist_reading, forecloses).
narrative_ontology:cs_reading_relation('605947d9-15fd-4a41-9553-0b808706386b', divine_marriage_command__coercion_visibility_reading, coexists_with).
narrative_ontology:cs_axiom('605947d9-15fd-4a41-9553-0b808706386b', foundational, plural_marriage_eternal_commandment).
narrative_ontology:cs_axiom_status(plural_marriage_eternal_commandment, holdable).
narrative_ontology:cs_axiom_grounding('605947d9-15fd-4a41-9553-0b808706386b', plural_marriage_eternal_commandment, deontological).
narrative_ontology:cs_axiom('605947d9-15fd-4a41-9553-0b808706386b', foundational, manifesto_prudential_suspension_only).
narrative_ontology:cs_axiom_status(manifesto_prudential_suspension_only, holdable).
narrative_ontology:cs_axiom_grounding('605947d9-15fd-4a41-9553-0b808706386b', manifesto_prudential_suspension_only, conventional).
narrative_ontology:cs_reference_frame('605947d9-15fd-4a41-9553-0b808706386b', original_revelation_authority).
narrative_ontology:cs_drift_state('605947d9-15fd-4a41-9553-0b808706386b', contemporary_institutional_posture, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('605947d9-15fd-4a41-9553-0b808706386b', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__continuationist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, mainstream_institutional_leadership).
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, fundamentalist_splinter_groups).
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, federal_regulatory_state).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, plural_marriage_practitioners).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, excluded_women_in_fundamentalist_communities).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, dissident_theologians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, fundamentalist_splinter_groups).
narrative_ontology:constraint_vindicates(divine_marriage_command__continuationist_reading, divine_command_continuity_doctrine).
narrative_ontology:constraint_vindicates(divine_marriage_command__continuationist_reading, revelation_immutability_principle).
narrative_ontology:constraint_vindicates(divine_marriage_command__continuationist_reading, prudential_suspension_distinction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the 1890/1904 Manifesto suspension as prudential policy while maintaining D&C 132 as binding revelation. Controls temple recommend standards, excommunication authority, and official curriculum. Benefits from federal accommodation (tax exemption, legal recognition) and from splinter groups absorbing the practice burden. Could revise doctrine but refuses — the doctrinal claim is the institution's immune system against total revision.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, mainstream_institutional_leadership, agenda_setter,
    institutional, generational, arbitrage, global).

% Practice plural marriage openly as 'the Principle,' claiming continuity with Joseph Smith's revelation. Extract tithing, labor, and reproductive compliance from members (especially women) through doctrinal authority. Their existence validates the mainstream's doctrinal claim while letting the mainstream disavow practice. Members are identity-locked: exit requires rejecting the core revelation that constitutes their community's legitimacy.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, fundamentalist_splinter_groups, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__continuationist_reading, fundamentalist_splinter_groups, payer).

% Achieved de facto monogamy enforcement through the Manifesto without having to adjudicate theological truth. Maintains criminal statutes (Utah Constitution, federal bigamy laws) that are selectively enforced. Benefits from a stable religious landscape where the mainstream institution polices its own boundaries. The constraint's persistence serves federal order without requiring doctrinal settlement.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, federal_regulatory_state, beneficiary,
    institutional, generational, arbitrage, national).

% Theologically believe plural marriage is a celestial requirement for exaltation. Legally criminalized (felony bigamy, cohabitation statutes). Cannot practice openly without prosecution; cannot abandon the practice without doctrinal apostasy that threatens eternal salvation. Exit requires either total community severance (losing family, economic network, identity) or doctrinal surrender (accepting the Manifesto as revelation, which the continuationist reading denies).
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, plural_marriage_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Bear the gendered costs of plural marriage: reproductive burden, economic dependency, hierarchical subordination, limited education, and legal vulnerability (no civil marriage protections). Have no voice in doctrinal interpretation — the continuationist reading is authored by male priesthood hierarchy. Exit is trapped: leaving means total severance from children, community, and eternal sealing theology; staying means ongoing extraction. Their situation is not represented in any beneficiary declaration.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, excluded_women_in_fundamentalist_communities, payer,
    powerless, biographical, trapped, local).

% Argue that the Manifesto represented genuine revelation superseding D&C 132, or that the continuationist distinction is incoherent. Face institutional discipline (disfellowship, excommunication, career termination at church institutions). Their exit is constrained: they can leave the institution but lose professional and social networks; they cannot reform the institution from within because the agenda_setter seat controls the interpretation machinery.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, dissident_theologians, excluded,
    moderate, biographical, constrained, global).

% Sees the full structure: a doctrinal claim suspended for 130+ years that generates ongoing extraction from practitioners while coordinating institutional survival, splinter-group legitimacy, and federal regulatory objectives. No stake in the outcome; the classification is the measurement.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates institutional survival under existential federal threat (1890) and ongoing regulatory stability by maintaining a doctrinal suspension that satisfies federal monogamy demands without conceding theological error.
% TRANSFER_FUNCTION: Moves compliance costs (legal risk, social stigma, reproductive burden, economic dependency) from the mainstream institution and federal state onto plural marriage practitioners and fundamentalist community members — especially women. Moves doctrinal legitimacy and identity coherence from practitioners to splinter groups and mainstream leadership.
% ABSENT_VOICES: Women in fundamentalist communities (especially second/third wives, daughters raised in the system) would object to the gendered extraction but are structurally excluded from doctrinal authorship and institutional authority. Children born into plural marriage have no voice in the arrangement that shapes their legal status and life options. Former practitioners who exited at high cost are excluded from the continuationist narrative that frames their exit as apostasy.
% DISAPPEARANCE_RATIONALE: If the continuationist constraint vanished overnight (doctrinal revision acknowledging the Manifesto as revelation, or federal decriminalization with theological settlement), the mainstream institution would face a legitimacy crisis (revelation mutability), splinter groups would lose their continuity claim, practitioners would gain legal recognition but lose theological coherence, and the federal state would lose its stable regulatory partner. The world rearranges because multiple seated parties' arrangements depend on the constraint's current form.
% FOUNDING_PROBLEM: Institutional survival under federal assault: the 1887 Edmunds-Tucker Act disincorporated the church, seized assets, and threatened total destruction. The 1890 Manifesto was the survival response.
% FOUNDING_PROBLEM_CORROBORATION: Federal legal historians (Sarah Barringer Gordon, Kathryn Daynes) attest the original federal threat (disincorporation, asset seizure, leadership imprisonment) was resolved by 1907 statehood and the Smoot hearings. The mainstream institution's own 1904 Second Manifesto acknowledged the 'new condition' of statehood. No credible historian outside the continuationist tradition argues the 1890 existential threat persists. The continuationist leadership self-attests the problem is live (ongoing cultural hostility); this self-attestation is the cover story the constraint rides on.
narrative_ontology:disappearance_verdict(divine_marriage_command__continuationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__continuationist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__continuationist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(divine_marriage_command__continuationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__continuationist_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__continuationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__continuationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint maintains a doctrinal claim that generates ongoing compliance costs for practitioners while the institution that could revise it refuses to — the doctrinal suspension creates a permanent extraction surface. Suppression (0.72) is higher because the constraint's persistence depends on active enforcement: mainstream discipline of dissenters, federal prosecution of practitioners, splinter group internal coercion. Theater ratio (0.41) is moderate: the doctrinal continuity claim performs theological coherence while the functional operation is institutional survival and regulatory management. Accessibility collapse (0.65) reflects that alternatives (doctrinal revision, open practice, exit) are structurally blocked — practitioners cannot legally practice, dissidents cannot revise doctrine, women in splinter groups cannot exit without total community severance. Resistance (0.58) is substantial: fundamentalist persistence, legal challenges, scholarly dissent, and member exits all register resistance, but the constraint endures.
 *
 * PERSPECTIVAL GAP:
 *   The mainstream leadership seat computes as beneficiary (d ~ 0.15): they control interpretation, avoid federal destruction, and offload practice costs to splinters. The practitioner seat computes as full target (d ~ 0.95): theologically bound, legally criminalized, identity-locked exit. The splinter seat computes as ambivalent beneficiary/target (d ~ 0.45): they claim continuity (benefit) but bear the full legal and social cost of practice (extraction). The federal seat computes as symmetric coordinator (d ~ 0.5): achieves policy goal without theological resolution. The engine computes this divergence from the structural data; the claimed_type does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: mainstream_institutional_leadership (controls doctrine, avoids existential threat), fundamentalist_splinter_groups (identity legitimacy, member compliance), federal_regulatory_state (policy objective achieved). Victims declared: plural_marriage_practitioners (bear legal/theological double bind), excluded_women_in_fundamentalist_communities (gendered extraction, no voice), dissident_theologians (disciplined for revisionist views). The directionality derivation chain reads these declarations + exit options: practitioners and splinter women are identity_locked/trapped → d near 1.0; leadership has arbitrage-grade exit (institutional survival) → d near 0.0; federal state has analytical distance → d ~ 0.5. The omega variables document the irreducible uncertainties in this mapping.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1890: institutional survival under federal assault) is dead — the federal threat that motivated the Manifesto has not existed in its original form for a century. Yet the arrangement persists and has elaborated: the doctrinal suspension became a permanent 'prudential' posture that extracts compliance from practitioners while preserving institutional and splinter-group interests. The constraint is not a scaffold (no sunset, no transition) but a tangled rope where the coordination function (institutional survival) has atrophied into a steady-state extraction mechanism. The continuationist reading prevents mislabeling this as pure coordination (rope) by maintaining the doctrinal claim that generates the extraction surface; it prevents mislabeling as pure extraction (snare) by preserving the genuine coordination benefit (federal accommodation, institutional continuity) that all seated parties except practitioners receive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the divine_marriage_command kernel, and does the continuationist reading''s ε reflect the standing arrangement of doctrinal continuity under contest?',
    'Cross-reading comparison: substitutionist_reading authors ε ≈ 0.2 (monogamy as new norm); coercion_visibility_reading authors ε ≈ 0.55 (survival necessity). Divergence confirms separate constraints.',
    'If ε values converge across readings, the kernel decomposition fails — the label ''divine marriage command'' would name one constraint, not three.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Reading-indexed ε over fixed referent (divine_marriage_command kernel)').

omega_variable(
    doctrinal_naturalness_ambiguity,
    'Is the doctrinal claim of polygamy''s eternal validity a genuine Mountain (irreducible revelation) or a constructed constraint benefiting institutional and splinter-group actors?',
    'Historical-theological analysis of whether the ''eternal command'' framing emerged simultaneously with the original revelation or was retrofitted during the 1890–1904 crisis period.',
    'If constructed, the constraint is a false summit — FSM would reclassify from mountain-claimed to tangled_rope via beneficiary presence (institutional_leadership, splinter_groups). If genuine Mountain, the continuationist reading''s high ε is an analytical error.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_naturalness_ambiguity, conceptual, 'Mountain naturalness vs. constructed doctrinal immunity').

omega_variable(
    splinter_continuity_legitimacy,
    'Do fundamentalist splinter groups genuinely represent doctrinal continuity, or is their claim a strategic identity frame that extracts compliance from members while the mainstream institution benefits from their existence as a pressure valve?',
    'Comparative analysis of splinter group internal coercion patterns vs. mainstream institutional statements on splinter groups; membership exit trajectory studies.',
    'If splinter continuity is performative, the constraint''s extraction is higher than measured — splinter groups become extraction amplifiers rather than genuine continuity witnesses.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(splinter_continuity_legitimacy, empirical, 'Splinter groups as genuine witnesses vs. extraction amplifiers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__continuationist_reading, 1890, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divine_marriage_command__continuationist_reading_tr_t1890, divine_marriage_command__continuationist_reading, theater_ratio, 1890, 0.15).
narrative_ontology:measurement(divine_marriage_command__continuationist_reading_tr_t1904, divine_marriage_command__continuationist_reading, theater_ratio, 1904, 0.28).
narrative_ontology:measurement(divine_marriage_command__continuationist_reading_tr_t1920, divine_marriage_command__continuationist_reading, theater_ratio, 1920, 0.35).
narrative_ontology:measurement(divine_marriage_command__continuationist_reading_tr_t1950, divine_marriage_command__continuationist_reading, theater_ratio, 1950, 0.38).
narrative_ontology:measurement(divine_marriage_command__continuationist_reading_tr_t1978, divine_marriage_command__continuationist_reading, theater_ratio, 1978, 0.4).
narrative_ontology:measurement(divine_marriage_command__continuationist_reading_tr_t2000, divine_marriage_command__continuationist_reading, theater_ratio, 2000, 0.41).
narrative_ontology:measurement(divine_marriage_command__continuationist_reading_tr_t2024, divine_marriage_command__continuationist_reading, theater_ratio, 2024, 0.41).

% Extraction over time
narrative_ontology:measurement(divine_marriage_command__continuationist_reading_be_t1890, divine_marriage_command__continuationist_reading, base_extractiveness, 1890, 0.45).
narrative_ontology:measurement(divine_marriage_command__continuationist_reading_be_t1904, divine_marriage_command__continuationist_reading, base_extractiveness, 1904, 0.52).
narrative_ontology:measurement(divine_marriage_command__continuationist_reading_be_t1920, divine_marriage_command__continuationist_reading, base_extractiveness, 1920, 0.58).
narrative_ontology:measurement(divine_marriage_command__continuationist_reading_be_t1950, divine_marriage_command__continuationist_reading, base_extractiveness, 1950, 0.62).
narrative_ontology:measurement(divine_marriage_command__continuationist_reading_be_t1978, divine_marriage_command__continuationist_reading, base_extractiveness, 1978, 0.65).
narrative_ontology:measurement(divine_marriage_command__continuationist_reading_be_t2000, divine_marriage_command__continuationist_reading, base_extractiveness, 2000, 0.67).
narrative_ontology:measurement(divine_marriage_command__continuationist_reading_be_t2024, divine_marriage_command__continuationist_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(divine_marriage_command__continuationist_reading_su_t1890, divine_marriage_command__continuationist_reading, suppression_requirement, 1890, 0.55).
narrative_ontology:measurement(divine_marriage_command__continuationist_reading_su_t1904, divine_marriage_command__continuationist_reading, suppression_requirement, 1904, 0.65).
narrative_ontology:measurement(divine_marriage_command__continuationist_reading_su_t1920, divine_marriage_command__continuationist_reading, suppression_requirement, 1920, 0.68).
narrative_ontology:measurement(divine_marriage_command__continuationist_reading_su_t1950, divine_marriage_command__continuationist_reading, suppression_requirement, 1950, 0.7).
narrative_ontology:measurement(divine_marriage_command__continuationist_reading_su_t1978, divine_marriage_command__continuationist_reading, suppression_requirement, 1978, 0.71).
narrative_ontology:measurement(divine_marriage_command__continuationist_reading_su_t2000, divine_marriage_command__continuationist_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(divine_marriage_command__continuationist_reading_su_t2024, divine_marriage_command__continuationist_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__continuationist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_marriage_command__continuationist_reading, 0.08).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, divine_marriage_command__substitutionist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, divine_marriage_command__coercion_visibility_reading).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, federal_anti_polygamy_enforcement).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, fundamentalist_community_governance).

% DUAL FORMULATION NOTE:
% This reading and substitutionist_reading are constraint siblings in the divine_marriage_command family. The ε-invariance principle requires separate stories: continuationist ε = 0.68 (doctrinal continuity generates ongoing extraction); substitutionist ε ≈ 0.20 (new revelation resolves the extraction by changing the command). They share the kernel but instantiate different constraints with different beneficiary/victim structures. The coercion_visibility_reading occupies an intermediate position (ε ≈ 0.55) where coercion is visible but doctrinal status remains ambiguous.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(divine_marriage_command__continuationist_reading, organized, 0.45).
constraint_indexing:directionality_override(divine_marriage_command__continuationist_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
