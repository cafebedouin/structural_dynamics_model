% ============================================================================
% CONSTRAINT STORY: indian_constitution_1950__social_revolution_provisions
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indian_constitution_1950__social_revolution_provisions, []).

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
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: indian_constitution_1950__social_revolution_provisions
 *   human_readable: Indian Constitution Article 17 & Equality Provisions: Social Revolution Against Caste Extraction
 *   domain: constitutional_law/social_policy/caste
 *
 * SUMMARY:
 *   The Indian Constitution's Articles 14–17 and the reservation system
 *   embody a structural attempt to abolish caste-based extraction and
 *   coordinate institutional inclusion of historically subordinated groups.
 *   Article 17 declares untouchability 'abolished and its practice in any
 *   form forbidden,' Articles 14–16 guarantee equality before law and forbid
 *   discrimination on caste grounds, and Part IV enumerates principles of
 *   social policy including caste prohibition. The constraint is tangled: it
 *   accomplishes genuine coordination (legal equality, institutional access
 *   through reservations) while failing to accomplish extraction suppression
 *   (caste disability persists socially despite legal prohibition). The
 *   measurement trajectory shows declining theater_ratio (from 0.92 in 1950
 *   to 0.48 in 2023) as implementation deepened and ritual prohibition became
 *   enforceable. Extractiveness declines (from 0.72 to 0.38) as reservation
 *   beneficiaries achieved critical mass institutional presence, and
 *   suppression declines (from 0.85 to 0.62) as caste-based violence, while
 *   persistent, faced legal consequences. The constraint is one reading of
 *   the contested kernel 'indian_constitution_1950,' alongside
 *   amendment_and_basic_structure, directive_principles_part_iv,
 *   federal_asymmetry, and fundamental_rights_part_iii. This reading
 *   instantiates the social revolution reading: the constitution legislates
 *   abolition of caste extraction through legal prohibition and institutional
 *   coordination mechanisms. The sibling readings contest whether this
 *   revolutionary mandate is the constitution's deepest logic or whether its
 *   flexibility, enforceable rights, or policy conscience are more
 *   fundamental.
 *
 * KEY AGENTS:
 *   - Historically Subordinated Castes (Harijans, Scheduled Castes): Primary beneficiary (powerless/trapped) — gain legal equality and institutional access; remain trapped by social fact of caste disability
 *   - Caste Extraction Apparatus (Brahminical Ritual Monopoly): Primary victim/extractor (institutional/constrained) — caste extraction declared illegal; extraction mechanism prohibited but institutional structure persists
 *   - Reservation Beneficiaries (Scheduled and Other Backward Classes): Secondary beneficiary (moderate/constrained) — gain institutional access through quotas; constrained by permanent statutory marking
 *   - Ambedkar Coalition (Constitutional Architects & Social Revolution Framers): Organized actor (organized/mobile) — design coordination mechanism for institutional transformation; see constraint as enabling collective access
 *   - Reform-Oriented Upper-Caste Elites: Secondary actor (powerful/mobile) — view social revolution as corrective scaffolding with sunset clause; mobile to influence amendments
 *   - Brahminical Ritual Establishment (Temples, Priesthoods): Institutional actor (institutional/constrained) — experience legal abolition as constraint on ritual monopoly; maintain de facto exclusion through alternative mechanisms
 *   - Analytical Observer (Civilizational Perspective): Observes structural inversion — text attempts to abolish extraction via legal prohibition; social fact persists despite legal change
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indian_constitution_1950__social_revolution_provisions, 0.38).
domain_priors:suppression_score(indian_constitution_1950__social_revolution_provisions, 0.62).
domain_priors:theater_ratio(indian_constitution_1950__social_revolution_provisions, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indian_constitution_1950__social_revolution_provisions, extractiveness, 0.38).
narrative_ontology:constraint_metric(indian_constitution_1950__social_revolution_provisions, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(indian_constitution_1950__social_revolution_provisions, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indian_constitution_1950__social_revolution_provisions, tangled_rope).
narrative_ontology:human_readable(indian_constitution_1950__social_revolution_provisions, "Indian Constitution Article 17 & Equality Provisions: Social Revolution Against Caste Extraction").
narrative_ontology:topic_domain(indian_constitution_1950__social_revolution_provisions, "constitutional_law/social_policy/caste").

domain_priors:requires_active_enforcement(indian_constitution_1950__social_revolution_provisions).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(indian_constitution_1950__social_revolution_provisions, 'ffed7479-67a9-4c71-9523-98e1bc902146').
narrative_ontology:cs_kernel_codification('ffed7479-67a9-4c71-9523-98e1bc902146', formalized).
narrative_ontology:cs_authority_grounding('ffed7479-67a9-4c71-9523-98e1bc902146', lineage).
narrative_ontology:cs_interpretation_layer_present('ffed7479-67a9-4c71-9523-98e1bc902146').
narrative_ontology:cs_reading_relation('ffed7479-67a9-4c71-9523-98e1bc902146', indian_constitution_1950__amendment_and_basic_structure, coexists_with).
narrative_ontology:cs_reading_relation('ffed7479-67a9-4c71-9523-98e1bc902146', indian_constitution_1950__fundamental_rights_part_iii, influences).
narrative_ontology:cs_reading_relation('ffed7479-67a9-4c71-9523-98e1bc902146', indian_constitution_1950__directive_principles_part_iv, coexists_with).
narrative_ontology:cs_reading_relation('ffed7479-67a9-4c71-9523-98e1bc902146', indian_constitution_1950__federal_asymmetry, influences).
narrative_ontology:cs_axiom('ffed7479-67a9-4c71-9523-98e1bc902146', foundational, caste_extraction_is_unconstitutional).
narrative_ontology:cs_axiom_status(caste_extraction_is_unconstitutional, holdable).
narrative_ontology:cs_axiom_grounding('ffed7479-67a9-4c71-9523-98e1bc902146', caste_extraction_is_unconstitutional, deontological).
narrative_ontology:cs_axiom('ffed7479-67a9-4c71-9523-98e1bc902146', foundational, institutional_access_through_positive_coordination).
narrative_ontology:cs_axiom_status(institutional_access_through_positive_coordination, holdable).
narrative_ontology:cs_axiom_grounding('ffed7479-67a9-4c71-9523-98e1bc902146', institutional_access_through_positive_coordination, instrumental).
narrative_ontology:cs_reference_frame('ffed7479-67a9-4c71-9523-98e1bc902146', constitutional_mandate_for_caste_abolition_and_social_equality).
narrative_ontology:cs_drift_state('ffed7479-67a9-4c71-9523-98e1bc902146', contemporary_2023, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ffed7479-67a9-4c71-9523-98e1bc902146', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(indian_constitution_1950__social_revolution_provisions, indian_constitution_1950).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(indian_constitution_1950__social_revolution_provisions, historically_subordinated_castes).
narrative_ontology:constraint_beneficiary(indian_constitution_1950__social_revolution_provisions, temple_entry_advocates).
narrative_ontology:constraint_beneficiary(indian_constitution_1950__social_revolution_provisions, reservation_beneficiaries).
narrative_ontology:constraint_victim(indian_constitution_1950__social_revolution_provisions, caste_extraction_apparatus).
narrative_ontology:constraint_victim(indian_constitution_1950__social_revolution_provisions, brahminical_privilege_holders).
narrative_ontology:constraint_victim(indian_constitution_1950__social_revolution_provisions, ritual_monopoly_defenders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNTOUCHABLE SUBJECT (SNARE) — Article 17 abolishes untouchability by constitutional decree, but the agent cannot exit the social fact of caste disability. Suppression remains structural: ritual pollution beliefs persist, temple entry guaranteed on paper but physically obstructed, caste names still mark identity. The constraint is constitutionally inverted (caste extraction declared illegal) but socially persistent. Maximum experienced extraction because the subject is trapped by inherited disability that no text can immediately erase.
constraint_indexing:constraint_classification(indian_constitution_1950__social_revolution_provisions, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SCHEDULED CASTE RESERVATION BENEFICIARY (TANGLED ROPE) — Experiences genuine coordination function: reservations solve the collective action problem of historical exclusion from institutions (education, employment, political office). But the mechanism is asymmetrically extractive: the beneficiary gains access at the cost of permanent statutory marking as 'scheduled,' identity codified in law. Constrained exit — cannot shed the legal category without abandoning benefit access. Mixed: real institutional gain paired with permanent categorical visibility.
constraint_indexing:constraint_classification(indian_constitution_1950__social_revolution_provisions, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: AMBEDKAR COALITION & SOCIAL REVOLUTION FRAMERS (ROPE) — The constitutional architects see the provisions as solving a collective coordination problem: how to legislate the social revolution against inherited caste disability without violent overthrow. The mechanism is coordination-dominant: reservations and anti-untouchability clauses coordinate institutional transformation. Low effective extraction from this perspective because the beneficiary is the historically excluded (who gain access), not a privileged class extracting rent. This perspective sees the constraint as enabling, not coercive.
constraint_indexing:constraint_classification(indian_constitution_1950__social_revolution_provisions, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORM-ORIENTED BRAHMIN ELITE (SCAFFOLD) — Forward-looking upper-caste actors see the constitutional social revolution as a temporary constraint on old privilege: a sunset clause written into the constitution itself. The provisions are meant to be corrective, not permanent — quotas expire when 'adequate representation' is achieved (Article 334 original termination, extended multiple times). This perspective expects the scaffolding to be dismantled once the social revolution succeeds. Low effective extraction experienced because the constraint has built-in expiration, and the agent has political mobility to influence amendments.
constraint_indexing:constraint_classification(indian_constitution_1950__social_revolution_provisions, scaffold,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: BRAHMINICAL RITUAL ESTABLISHMENT (PITON) — For the institutional structure of ritual monopoly and temple gatekeeping, the constitutional provisions are constitutionally nullified but institutionally persistent. Article 17 declares untouchability void but many temples practice de facto exclusion through alternative mechanisms (purity rules, entry fees for non-brahmin priests). The constraint is largely performative — the text is revolutionary but the enforcement is minimal, and the ritual apparatus persists through institutional inertia. Theater_ratio high because the text abolishes untouchability but ritual practice continues unchanged, maintained by custom rather than legal authority.
constraint_indexing:constraint_classification(indian_constitution_1950__social_revolution_provisions, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: BRAHMINICAL PRIVILEGE APPARATUS (SNARE) — From the perspective of the caste extraction system itself, the constitutional provisions are attempted extraction suppression: Article 17 declares untouchability void, Article 14 guarantees equality, Article 15 forbids discrimination on caste grounds, Article 16 opens public employment. The old apparatus experiences this as a structural snare — it cannot legally extract through caste disabilities anymore, but the institutional structure persists (brahmin overrepresentation in judiciary, bureaucracy, temples). The extraction mechanism is prohibited but the apparatus remains. Constrained exit — the institution cannot fully exit (caste structure is civilizational) but can no longer legally extract.
constraint_indexing:constraint_classification(indian_constitution_1950__social_revolution_provisions, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — The constraint exhibits structural inversion: a text that attempts to abolish extraction (caste disabilities, ritual monopoly) by declaring it illegal, yet the extracted population cannot exit the social fact of caste even under the new law. The coordination function is real (reservations solve institutional exclusion). The extraction continues (suppression, inequality persists despite legal prohibition). This is tangled rope from the civilizational view because the constitutional text accomplishes genuine coordination (legal equality, institutional access) while failing to accomplish extraction suppression (caste disability persists socially). The inversion is itself the constraint: the law declares extraction illegal while the social structure persists.
constraint_indexing:constraint_classification(indian_constitution_1950__social_revolution_provisions, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indian_constitution_1950__social_revolution_provisions_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(indian_constitution_1950__social_revolution_provisions, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indian_constitution_1950__social_revolution_provisions, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(indian_constitution_1950__social_revolution_provisions, TR),
    TR >= 0.70.

:- end_tests(indian_constitution_1950__social_revolution_provisions_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38 at t=73): Moderate, declining from 0.72 at constitution's adoption. Initial extractiveness high because the caste system extracted substantial rents from subordinated groups — ritual labor obligations, economic dependence, occupational exclusion. The constitutional provisions declare this extraction illegal but cannot immediately erase the social fact. By 2023, extractiveness has declined as reservation beneficiaries achieved critical institutional mass, as caste-based violence faced legal consequences, and as caste identity became politically mobilized rather than purely suppressive. The constraint remains extractive (not rope-level) because caste disability persists: occupational segregation, educational inequality, ritual exclusion, and violence continue despite legal prohibition. Suppression (0.62 at t=73): Moderate-high, declining from 0.85. Initial suppression reflected the total suppression of caste disability — alternatives to caste status did not exist. By 2023, suppression has declined but remains substantial: exit from caste identity is structurally available (through education, geographic mobility, inter-caste marriage) but socially costly. The caste structure persists despite legal prohibition because it is internalized, identity-locked, and reinforced by centuries of practice. Theater ratio (0.48 at t=73): Moderate, declining from 0.92. Initial theater very high because Article 17 and equality provisions were revolutionary text with minimal implementation — temples continued de facto exclusion, bureaucracy remained brahmin-dominated, ritual monopoly persisted. By 2023, theater has declined because implementation deepened: reserved category appointments visible, caste violence prosecutions public, temple entry litigated and partially enforced. The theater persists (not 0.0) because gaps between legal prohibition and social reality remain substantial.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival gap. From the trapped untouchable's view, the constitutional provisions are snare-like (legal equality does not eliminate social disability). From the reservation beneficiary's view, they are tangled_rope (genuine institutional access paired with permanent marking). From the Ambedkar coalition's view, they are rope (solving a coordination problem). From the reform elite's view, they are scaffold (temporary correction of old privilege). From the ritual establishment's view, they are piton (legal text with minimal enforcement). From the brahminical apparatus's view, they are snare (extraction mechanism prohibited but institutional structure persists). From the analytical observer's civilizational view, they are tangled_rope (genuine coordination paired with failed extraction suppression). The six perspectives collapse to three distinct clusters: snare (trapped powerless agents), tangled_rope (mixed coordination + extraction for moderate/organized/analytical actors), and scaffold/piton (performative or temporary for privileged actors). The gap arises because the constraint inverts the extraction flow — the legal text declares the extraction illegal, but the social fact persists. No single agent sees the constraint as purely beneficial (rope) or purely coercive (snare) except the extremes.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from the constraint's inversion: it attempts to suppress caste extraction by legal prohibition and institutional coordination (reservations). For beneficiaries (historically subordinated castes), d is high (0.85–0.95) because they bear the social fact of caste despite legal abolition — the constraint suppresses their extraction capacity in practice even as it legally prohibits extraction against them. For the caste extraction apparatus, d is low-to-moderate (0.15–0.40) because the constraint's text prohibits them from extracting but the social structure persists, giving them partial veto capacity over implementation. For analytical observers, d is moderate (0.70) because the constraint targets extraction but fails to suppress it. The directionality computations flow from this structural reality: beneficiaries experience high chi (high suppression, high powerlessness), the extraction apparatus experiences low chi (legal prohibition even if social persistence), organized/moderate actors experience moderate chi (genuine institutional access paired with residual extraction). No directionality overrides needed — the structural derivation captures the constraint's logical inversion.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question: Is this a rope (pure coordination) or a snare (pure extraction)? The answer is neither. The constitutional provisions are not pure coordination because caste disability persists despite legal prohibition — the trapped agent cannot exit. They are not pure extraction because genuine institutional access occurs through reservations and legal equality — the beneficiary gains real access. The constraint is tangled_rope: it accomplishes genuine coordination (solves the institutional exclusion problem) while failing to accomplish extraction suppression (caste disability persists). The mandatrophy resolves by observing that the classification depends on which question you ask: 'Does the constraint solve institutional access?' (Yes, rope-like.) 'Does the constraint eliminate caste extraction?' (No, snare-like.) The true answer is both simultaneously — tangled. The resolution does not depend on hidden measurement methodology or observer selection; it depends on which structural outcome you measure. Institutional parity vs social equality are different questions with different measurement timescales. The constraint accomplishes the first partially (reservations achieved visible representation but not outcome parity) and fails the second substantially (caste disability persists). This is structural, not epistemic. The tangled_rope classification is therefore not a fallback or a hedge — it is the precise description of a constraint that mixes coordination and extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_text_versus_social_fact_gap,
    'How much does the constitutional abolition of caste disability close the social fact of caste suppression? Is Article 17 a mountain of legal principle or a rope that coordinates institutional transformation?',
    'Longitudinal measurement of caste-based violence, ritual exclusion, economic inequality, educational access, and occupational segregation before and after 1950; cross-country comparison with constitutional equality provisions in other post-colonial democracies; analysis of implementation gaps between statute and practice',
    'If text successfully suppresses extraction (caste disabilities decline toward zero): mountain from institutional perspective (legal prohibition = structural change). If social fact persists despite text: tangled rope (legal prohibition + social persistence = mixed constraint).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_text_versus_social_fact_gap, empirical, 'Gap between constitutional prohibition of caste and social persistence of caste disability').

omega_variable(
    reservation_mechanism_as_coordinate_or_extraction,
    'Do reservations coordinate institutional inclusion (beneficiary perspective: rope) or extract from unreserved candidates through reduced competition (victimized perspective: snare)?',
    'Analysis of actual outcomes: Do reserved candidates achieve parity in outcomes post-graduation? Do unreserved-category candidates face measurable exclusion? Comparative analysis of social mobility trajectories by caste with and without reservations; analysis of creamy layer exclusions and their effects',
    'If reservations achieve parity: coordinate inclusion (rope dominant). If persistent gaps or backlash extraction: tangled rope. If permanent identity marking without outcome equality: snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reservation_mechanism_as_coordinate_or_extraction, empirical, 'Whether reservations coordinate inclusion or extract costs from unreserved candidates').

omega_variable(
    reading_contest_basic_structure_constraint,
    'Which reading is operative in practice: social_revolution_provisions (this reading) or amendment_and_basic_structure (sibling)? Does the basic structure doctrine protect caste abolition, or does it function as a brake on social revolution deepening?',
    'Analysis of Supreme Court jurisprudence on caste and reservations: has the basic structure protected Article 17 and equality provisions, or has it limited the scope of social revolution provisions through counter-majoritarian constraints? Case law on reservation expansion, caste-based violence remedies, and constitutional amendments affecting caste policy.',
    'If basic structure PROTECTS social revolution: social_revolution_provisions reading is fortified. If basic structure LIMITS social revolution: amendment_and_basic_structure reading dominates, and this reading''s extractiveness may decline (constraint weakened by judicial counter-revolution).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_basic_structure_constraint, conceptual, 'Whether basic structure doctrine protects or constrains the social revolution reading').

omega_variable(
    mandate_for_what_transformation,
    'Is the social revolution reading a mandate for immediate abolition of caste (structural transformation) or a mandate for legal equality with gradual social change (coordinative scaffolding)?',
    'Constitutional text analysis (Preamble, Article 1 affirmation of ''Republic''), Constituent Assembly debates on articles 14–17 and Part IV, Ambedkar''s statements on social revolution vs gradualism, judicial interpretation of scope and timeline for caste elimination',
    'If transformation mandate: extractiveness understated (should be higher — constraint is declaring caste illegal, not gradually removing it). If scaffolding mandate: extractiveness correctly calibrated (constraint is coordinative, not abolitionist). Reading''s classification may shift from tangled_rope toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_for_what_transformation, conceptual, 'Whether the social revolution is a mandate for immediate transformation or gradual coordinative change').

omega_variable(
    kernel_reading_contest_over_caste_constitutionalism,
    'This constraint is one reading of the contested kernel ''indian_constitution_1950.'' Is the Constitution''s deepest meaning its social revolution provisions (this reading), its amendment flexibility (sibling: amendment_and_basic_structure), its enforceable rights (sibling: fundamental_rights_part_iii), or its policy conscience (sibling: directive_principles_part_iv)?',
    'Jurisprudential analysis: which reading has been most generative in constitutional law? Track Supreme Court citations to Article 17 vs Article 368 (amendment), Part III vs Part IV, across major cases. Analyze which reading''s logic constrains the others.',
    'If social revolution reading is dominant: caste abolition drives the constitution''s logic, and extractiveness is core to its meaning. If amendment reading dominates: the constitution''s flexibility to modify social revolution provisions is its defining feature. If fundamental rights reading dominates: individual rights override collective caste abolition logic. If directive principles dominates: social revolution is aspirational, not enforceable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_over_caste_constitutionalism, conceptual, 'Which reading of the Indian Constitution kernel is most operative in jurisprudence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indian_constitution_1950__social_revolution_provisions, 0, 73).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ind_const_soc_rev_theater_1950, indian_constitution_1950__social_revolution_provisions, theater_ratio, 0, 0.92).
narrative_ontology:measurement(ind_const_soc_rev_theater_1975, indian_constitution_1950__social_revolution_provisions, theater_ratio, 25, 0.78).
narrative_ontology:measurement(ind_const_soc_rev_theater_2000, indian_constitution_1950__social_revolution_provisions, theater_ratio, 50, 0.62).
narrative_ontology:measurement(ind_const_soc_rev_theater_2023, indian_constitution_1950__social_revolution_provisions, theater_ratio, 73, 0.48).

% Extraction over time
narrative_ontology:measurement(ind_const_soc_rev_extractiveness_1950, indian_constitution_1950__social_revolution_provisions, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(ind_const_soc_rev_extractiveness_1975, indian_constitution_1950__social_revolution_provisions, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(ind_const_soc_rev_extractiveness_2000, indian_constitution_1950__social_revolution_provisions, base_extractiveness, 50, 0.42).
narrative_ontology:measurement(ind_const_soc_rev_extractiveness_2023, indian_constitution_1950__social_revolution_provisions, base_extractiveness, 73, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(ind_const_soc_rev_suppression_1950, indian_constitution_1950__social_revolution_provisions, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(ind_const_soc_rev_suppression_1975, indian_constitution_1950__social_revolution_provisions, suppression_requirement, 25, 0.75).
narrative_ontology:measurement(ind_const_soc_rev_suppression_2000, indian_constitution_1950__social_revolution_provisions, suppression_requirement, 50, 0.68).
narrative_ontology:measurement(ind_const_soc_rev_suppression_2023, indian_constitution_1950__social_revolution_provisions, suppression_requirement, 73, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indian_constitution_1950__social_revolution_provisions, identity_coordination).
narrative_ontology:affects_constraint(indian_constitution_1950__social_revolution_provisions, indian_constitution_1950__amendment_and_basic_structure).
narrative_ontology:affects_constraint(indian_constitution_1950__social_revolution_provisions, indian_constitution_1950__fundamental_rights_part_iii).
narrative_ontology:affects_constraint(indian_constitution_1950__social_revolution_provisions, indian_constitution_1950__directive_principles_part_iv).
narrative_ontology:affects_constraint(indian_constitution_1950__social_revolution_provisions, indian_constitution_1950__federal_asymmetry).
narrative_ontology:affects_constraint(indian_constitution_1950__social_revolution_provisions, caste_based_violence_persistence).
narrative_ontology:affects_constraint(indian_constitution_1950__social_revolution_provisions, reservation_creamy_layer_dynamics).

% DUAL FORMULATION NOTE:
% This constraint represents the social_revolution_provisions reading of the contested kernel indian_constitution_1950. It is linked to four sibling readings (amendment_and_basic_structure, fundamental_rights_part_iii, directive_principles_part_iv, federal_asymmetry) representing alternative interpretations of the Constitution's deepest meaning. Each reading has its own extractiveness value and classification. This story focuses on the caste abolition reading; see sibling constraint files for the amendment flexibility, enforceable rights, policy principles, and federal structure readings. Downstream constraints (caste_based_violence_persistence, reservation_creamy_layer_dynamics) inherit the social revolution reading's structure but measure different implementation gaps.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
