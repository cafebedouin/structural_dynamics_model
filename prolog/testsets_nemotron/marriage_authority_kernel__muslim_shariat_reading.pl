% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__muslim_shariat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-28
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__muslim_shariat_reading, []).

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
 *   constraint_id: marriage_authority_kernel__muslim_shariat_reading
 *   human_readable: Muslim Personal Law Authority over Marriage/Family (Shariat/Qazi Interpretation)
 *   domain: constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   Muslim personal law in India derives marriage/family authority from
 *   Shariat as interpreted by Muslim personal law boards and qazis (community
 *   judges). This reading of the marriage_authority_kernel maintains
 *   community tribunals that adjudicate divorce (including unilateral triple
 *   talaq until 2019), polygamy, maintenance, and inheritance with lower
 *   gender equity than codified Hindu law or secular civil law. State
 *   intervention is constitutionally contested — Article 44's directive for a
 *   Uniform Civil Code conflicts with Article 25-26 religious freedom
 *   guarantees. The constraint presents itself as divine mandate (Mountain
 *   claim) but operates through institutional interpretation that benefits
 *   male household heads and law board authority (tangled_rope structure).
 *
 * KEY AGENTS:
 *   - muslim_personal_law_boards: Primary beneficiary (institutional/identity_locked) — controls interpretation, derives authority from constraint
 *   - male_head_of_household: Primary beneficiary (powerful/identity_locked) — holds unilateral divorce, polygamy, inheritance advantages
 *   - muslim_women: Primary victim (powerless/identity_locked) — bears asymmetric divorce, maintenance, inheritance rules; exit blocked by identity and community pressure
 *   - muslim_children: Victim (powerless/identity_locked) — subject to guardianship and inheritance rules without voice
 *   - qazis: Agenda setter (organized/constrained) — administer tribunals, interpret Shariat, depend on law boards for legitimacy
 *   - state_courts: Observer/agenda_setter (institutional/analytical) — appellate review, constitutional challenges, legislative reform capacity
 *   - secular_rights_activists: Excluded (organized/mobile) — advocate for gender equality and UCC; structurally excluded from communal adjudication
 *   - muslim_reformist_scholars: Excluded (moderate/constrained) — argue for egalitarian interpretation from within tradition; marginalized by law boards
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__muslim_shariat_reading, 0.68).
domain_priors:suppression_score(marriage_authority_kernel__muslim_shariat_reading, 0.71).
domain_priors:theater_ratio(marriage_authority_kernel__muslim_shariat_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__muslim_shariat_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__muslim_shariat_reading, "Muslim Personal Law Authority over Marriage/Family (Shariat/Qazi Interpretation)").
narrative_ontology:topic_domain(marriage_authority_kernel__muslim_shariat_reading, "constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__muslim_shariat_reading, 'addb0eba-3c09-497e-a6dc-49b6bd45b969').
narrative_ontology:cs_kernel_codification('addb0eba-3c09-497e-a6dc-49b6bd45b969', fixed_text).
narrative_ontology:cs_authority_grounding('addb0eba-3c09-497e-a6dc-49b6bd45b969', lineage).
narrative_ontology:cs_interpretation_layer_present('addb0eba-3c09-497e-a6dc-49b6bd45b969').
narrative_ontology:cs_reading_relation('addb0eba-3c09-497e-a6dc-49b6bd45b969', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('addb0eba-3c09-497e-a6dc-49b6bd45b969', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('addb0eba-3c09-497e-a6dc-49b6bd45b969', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('addb0eba-3c09-497e-a6dc-49b6bd45b969', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('addb0eba-3c09-497e-a6dc-49b6bd45b969', foundational, shariat_interpretive_monopoly).
narrative_ontology:cs_axiom_status(shariat_interpretive_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('addb0eba-3c09-497e-a6dc-49b6bd45b969', shariat_interpretive_monopoly, theological).
narrative_ontology:cs_axiom('addb0eba-3c09-497e-a6dc-49b6bd45b969', foundational, communal_autonomy_over_individual_rights).
narrative_ontology:cs_axiom_status(communal_autonomy_over_individual_rights, holdable).
narrative_ontology:cs_axiom_grounding('addb0eba-3c09-497e-a6dc-49b6bd45b969', communal_autonomy_over_individual_rights, conventional).
narrative_ontology:cs_axiom('addb0eba-3c09-497e-a6dc-49b6bd45b969', foundational, male_guardianship_as_divine_order).
narrative_ontology:cs_axiom_status(male_guardianship_as_divine_order, holdable).
narrative_ontology:cs_axiom_grounding('addb0eba-3c09-497e-a6dc-49b6bd45b969', male_guardianship_as_divine_order, theological).
narrative_ontology:cs_reference_frame('addb0eba-3c09-497e-a6dc-49b6bd45b969', classical_fiqh_personal_law).
narrative_ontology:cs_drift_state('addb0eba-3c09-497e-a6dc-49b6bd45b969', post_shayara_bano_2017, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('addb0eba-3c09-497e-a6dc-49b6bd45b969', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, muslim_personal_law_boards).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, male_head_of_household).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, muslim_women).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, muslim_children).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__muslim_shariat_reading, religious_autonomy_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__muslim_shariat_reading, communal_self_governance_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control authoritative interpretation of Shariat for marriage/family matters. Derive institutional legitimacy, resources, and political influence from this monopoly. Resist state codification and feminist reinterpretation. Exit would mean surrendering the communal authority that constitutes their institutional identity.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, muslim_personal_law_boards, beneficiary,
    institutional, generational, identity_locked, national).

% Holds unilateral talaq (until 2019), polygamy rights, superior inheritance shares, and guardianship authority. These rights are exercised within the family but backed by qazi courts and law boards. Exit from the constraint means losing these privileges; identity as 'head of household' under Shariat is fused with the rights themselves.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, male_head_of_household, beneficiary,
    powerful, biographical, identity_locked, local).

% Subject to unilateral divorce, restricted maintenance (post-1985 Shah Bano legislation limited but did not equalize), half-inheritance shares, and subordinate child custody. Legal alternatives exist (Special Marriage Act, civil courts) but carry severe social ostracism, family rupture, and community exclusion. Identity as 'Muslim woman' is constructed through the constraint; exit is existentially costly.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, muslim_women, payer,
    powerless, biographical, identity_locked, local).

% Subject to paternal guardianship, asymmetric inheritance, and custody rules favoring fathers. No independent voice in qazi proceedings. Exit requires adulthood and community departure — the constraint shapes their legal personality from birth.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, muslim_children, payer,
    powerless, biographical, identity_locked, local).

% Adjudicate marriage/family disputes in community tribunals. Derive authority from law boards and communal recognition. Their role depends on the constraint's existence; they have professional investment in Shariat interpretation but limited autonomy — law boards set doctrinal boundaries. Exit means leaving religious adjudication for secular legal practice.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, qazis, agenda_setter,
    organized, biographical, constrained, local).

% Exercise appellate review over qazi decisions; hear constitutional challenges (Articles 14, 15, 21 vs 25-26); can strike down specific practices (triple talaq 2017/2019) but not the personal law system itself. Political constraints limit intervention. Their analytical seat lets them see the full structure; their institutional seat gives them reform capacity they rarely use fully.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, state_courts, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__muslim_shariat_reading, state_courts, agenda_setter).

% Advocate for Uniform Civil Code, gender equality, and individual rights over communal autonomy. Litigate, campaign, and lobby for legislative reform. Structurally excluded from qazi proceedings and law board deliberations. Their exit is mobile — they operate in constitutional/legislative arena — but their voices are excluded from the constraint's internal operation.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, secular_rights_activists, excluded,
    organized, generational, mobile, national).

% Argue for egalitarian Shariat interpretation (Quranic ethics over fiqh rulings), support codification, oppose triple talaq and polygamy. Marginalized by law boards; denied platforms in communal institutions. Exit is constrained — they remain within the tradition but are silenced by the authority structure. Their exclusion is internal to the community, not external like secular activists.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, muslim_reformist_scholars, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides community-based dispute resolution for marriage/family matters that is accessible, culturally legible, and faster than civil courts; maintains communal cohesion and religious identity in a plural legal order.
% TRANSFER_FUNCTION: Moves substantive legal rights (divorce initiation, maintenance, inheritance, child custody, polygamy permission) from women and children to men and communal authorities; moves interpretive authority from state to religious bodies.
% ABSENT_VOICES: Muslim women and children are physically present in qazi proceedings but structurally silenced — they cannot initiate divorce unilaterally, their testimony is weighted half, their preferences in custody are subordinate. Muslim reformist scholars and secular rights activists are excluded from law board deliberations and qazi appointments. Their objections would challenge the gender hierarchy and communal monopoly.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight, Muslim marriage/family disputes would shift to civil courts or the Special Marriage Act; women would gain equal divorce, maintenance, inheritance, and custody rights; law boards and qazis would lose institutional authority; the constitutional pluralism framework would collapse toward UCC. The social world of Muslim family life would fundamentally rearrange.
% FOUNDING_PROBLEM: Colonial and post-colonial need to govern a religiously plural society without imposing a single legal code; Muslim community demand for autonomy in personal law as condition of political participation; British policy of non-interference in 'religious' matters to secure loyalty.
% FOUNDING_PROBLEM_CORROBORATION: Law boards attest the problem is live: communal autonomy remains essential to Muslim identity in a Hindu-majority state. Feminist historians (Flavia Agnes, Zoya Hasan) and Supreme Court judgments (Shah Bano, Shayara Bano) attest the founding problem has shifted: the arrangement now primarily maintains gender hierarchy, not communal survival. The 2019 triple talaq ban (legislative, not communal) corroborates that state intervention is possible when political will exists — the 'non-interference' founding premise is overridden by gender equality imperatives.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__muslim_shariat_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__muslim_shariat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__muslim_shariat_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(marriage_authority_kernel__muslim_shariat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__muslim_shariat_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__muslim_shariat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__muslim_shariat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__muslim_shariat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint transfers substantive rights (divorce initiation, maintenance, inheritance, child custody) from women to men and from individuals to communal authorities. Suppression (0.71) is high because exit requires leaving the community identity (identity_locked), and state alternatives (Special Marriage Act) carry severe social ostracism costs. Theater ratio (0.22) is moderate-low: qazi courts provide real dispute resolution (coordination function), but a growing share of activity defends gender-asymmetric rules rather than resolving disputes. Accessibility collapse (0.62) reflects that alternatives exist legally (Special Marriage Act, civil courts) but are socially inaccessible for most. Resistance (0.58) is substantial: reform movements, litigation, and legislative action (2019 triple talaq ban) show active contestation. The claimed_type 'tangled_rope' captures the genuine coordination (community dispute resolution) fused with asymmetric extraction (gender hierarchy).
 *
 * PERSPECTIVAL GAP:
 *   From the law board/qazi seat, the constraint is a rope: it coordinates community dispute resolution and preserves religious autonomy. From the muslim_women seat, it is a snare: it extracts rights with no viable exit. From the state_courts seat, it is a contested scaffold: constitutional pluralism demands respect, but gender equality demands reform. The engine computes these per-seat classifications from the structural data; the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   muslim_personal_law_boards and male_head_of_household are structural beneficiaries (d ~ 0.15-0.25): they collect authority/rights from the constraint. muslim_women and muslim_children are structural victims (d ~ 0.85-0.95): they bear the extraction with identity_locked exit. qazis are agenda_setters with constrained exit (d ~ 0.5): they administer but depend on the system for role legitimacy. state_courts are observers with analytical exit (d ~ 0.5): they can review but face political constraints. secular_rights_activists and muslim_reformist_scholars are excluded — their voices would change the constraint but they hold no structural position within it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (communal self-governance under colonial/post-colonial pluralism) is contested: law boards say it remains live; reformers and courts say it has shifted to gender hierarchy maintenance. The constraint persists because the agenda_setters (law boards) benefit enough to defend it, but the payers (muslim_women) are too identity-locked to force change. State could fix it (legislative competence) but political cost is prohibitive — classic mandatrophy where the arrangement outlives its coordination function but no actor has both incentive and capacity to replace it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the marriage_authority_kernel, and what does the muslim_shariat_reading structurally change relative to sibling readings?',
    'Compare the victim/beneficiary structures, enforcement mechanisms, and gender equity profiles across all five declared readings. The muslim_shariat_reading uniquely features community tribunals (qazis), unilateral talaq, and contested state intervention — structural deltas not present in codified or canonical readings.',
    'Confirms this reading as a separate constraint with its own ε, stakeholders, and classification. Prevents conflation with hindu_codified_reading (state-court adjudication) or secular_civil_reading (individual-rights grounding).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This reading instantiates the marriage_authority_kernel as muslim_shariat_reading; sibling readings are hindu_codified_reading, christian_canonical_reading, parsi_communal_reading, secular_civil_reading. Structural delta: community tribunals adjudicate; lower gender equity (unilateral talaq, polygamy, inheritance); state intervention contested.').

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the Shariat-based authority a genuine natural/religious law (Mountain) or a constructed constraint maintained by identifiable beneficiaries?',
    'Trace historical codification of Muslim personal law in colonial and post-colonial India; examine whether qazis and law boards exercise interpretive discretion that benefits male household heads; assess whether state non-intervention is a structural choice or a genuine legal immunity.',
    'If constructed, the claimed_type ''tangled_rope'' is validated and false_summit_mountain risk is real. If natural law, extraction and suppression metrics must be near-zero — current values (0.68/0.71) contradict Mountain classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, conceptual, 'Natural-law vs. constructed ambiguity for a reading that presents itself as divine mandate but operates through institutional interpretation.').

omega_variable(
    state_intervention_contestation,
    'Does state non-intervention reflect constitutional pluralism or capitulation to communal power?',
    'Analyze Supreme Court jurisprudence (Shah Bano, Shayara Bano, triple talaq legislation); measure legislative capacity to reform vs. political cost of intervention; track whether contestation is doctrinal or interest-based.',
    'If capitulation, the constraint''s suppression (0.71) includes state enforcement of communal authority — the state becomes an active enforcer of extraction. If pluralism, suppression is internal to community; state role is passive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_intervention_contestation, preference, 'Whether the contested state intervention dynamic is a feature of constitutional design or a failure of state capacity/will.').

omega_variable(
    coordination_extraction_boundary,
    'Does the community tribunal system provide genuine coordination (dispute resolution, social stability) inseparable from its extractive gender provisions, or are they separable?',
    'Compare outcomes in communities with active qazi courts vs. those using civil courts; assess whether women''s rights reforms in other personal law systems degraded coordination function; examine if qazi courts deliver speed/access benefits that civil courts do not.',
    'If inseparable, the tangled_rope classification holds — genuine coordination AND asymmetric extraction are fused. If separable, the extraction component is a snare riding on a rope; decomposition into two constraints would be warranted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the coordination function of community adjudication can be preserved without the gender-asymmetric extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__muslim_shariat_reading, 1937, 2017).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marriage_authority_muslim_tr_t1937, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 1937, 0.1).
narrative_ontology:measurement(marriage_authority_muslim_tr_t1950, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(marriage_authority_muslim_tr_t1973, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 1973, 0.15).
narrative_ontology:measurement(marriage_authority_muslim_tr_t1985, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 1985, 0.18).
narrative_ontology:measurement(marriage_authority_muslim_tr_t2001, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 2001, 0.2).
narrative_ontology:measurement(marriage_authority_muslim_tr_t2017, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 2017, 0.22).

% Extraction over time
narrative_ontology:measurement(marriage_authority_muslim_be_t1937, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 1937, 0.45).
narrative_ontology:measurement(marriage_authority_muslim_be_t1950, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 1950, 0.52).
narrative_ontology:measurement(marriage_authority_muslim_be_t1973, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 1973, 0.55).
narrative_ontology:measurement(marriage_authority_muslim_be_t1985, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 1985, 0.62).
narrative_ontology:measurement(marriage_authority_muslim_be_t2001, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 2001, 0.65).
narrative_ontology:measurement(marriage_authority_muslim_be_t2017, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 2017, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(marriage_authority_muslim_su_t1937, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 1937, 0.4).
narrative_ontology:measurement(marriage_authority_muslim_su_t1950, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 1950, 0.5).
narrative_ontology:measurement(marriage_authority_muslim_su_t1973, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 1973, 0.55).
narrative_ontology:measurement(marriage_authority_muslim_su_t1985, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 1985, 0.62).
narrative_ontology:measurement(marriage_authority_muslim_su_t2001, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 2001, 0.68).
narrative_ontology:measurement(marriage_authority_muslim_su_t2017, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 2017, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__muslim_shariat_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__muslim_shariat_reading, 0.1).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__secular_civil_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, uniform_civil_code_directive).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, triple_talaq_legislation_2019).

% DUAL FORMULATION NOTE:
% The marriage_authority_kernel decomposes into five constraint stories, one per reading. This reading (muslim_shariat_reading) has the highest extraction (0.68) and suppression (0.71) because it uniquely combines community tribunal adjudication with gender-asymmetric provisions and contested state immunity. The hindu_codified_reading and secular_civil_reading approach rope/mountain (low extraction, state-enforced equality). The christian_canonical_reading and parsi_communal_reading sit between. All five are linked via affects_constraints to model the constitutional pluralism system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority_kernel__muslim_shariat_reading, institutional, 0.2).
constraint_indexing:directionality_override(marriage_authority_kernel__muslim_shariat_reading, powerful, 0.25).
constraint_indexing:directionality_override(marriage_authority_kernel__muslim_shariat_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
