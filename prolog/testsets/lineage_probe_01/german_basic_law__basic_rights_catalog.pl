% ============================================================================
% CONSTRAINT STORY: german_basic_law__basic_rights_catalog
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_german_basic_law__basic_rights_catalog, []).

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
 *   constraint_id: german_basic_law__basic_rights_catalog
 *   human_readable: German Basic Law: Rights Catalog (Articles 1–19)
 *   domain: political/legal/constitutional
 *
 * SUMMARY:
 *   The Basic Law's Articles 1–19 establish a constitutional binding on all
 *   state organs, placing fundamental rights at the constitutional apex and
 *   shielding their essential content from amendment via Article 79(3). This
 *   reading instantiates the basic_rights_catalog understanding of German
 *   constitutionalism: the constitution is NOT primarily a record of managed
 *   amendment (amendment_history reading), NOT primarily Article 1's
 *   inviolable dignity alone (dignity_and_eternity reading), NOT primarily
 *   federalism (federal_construction reading), and NOT primarily
 *   anti-democratic defense mechanisms (militant_democracy reading). Rather,
 *   it is a comprehensive, binding catalog of rights that directly constrains
 *   all state action and provides justiciable limits on state power. The
 *   constraint exhibits low extractiveness (0.28) because the rights
 *   catalog's function is to prevent extraction by making all state power
 *   proportional and rights-justified. The constraint's beneficiary set is
 *   universal — every rights-holder, citizen and resident alike. The victim
 *   set is administrative power that lacks justification grounded in rights.
 *   The theater ratio is low (0.35) because rights are enforced through
 *   constitutional courts with transparent, rule-based review.
 *
 * KEY AGENTS:
 *   - All Rights-Holders (Citizens and Residents): Primary beneficiary (powerless/constrained) — Articles 1–19 provide direct protection against state action with no exit from the guarantee
 *   - State Administration (Executive and Legislative): Secondary actor (moderate/constrained) — Bound by the rights catalog; must justify action through proportionality; benefits from legitimate authority grounded in rights justification
 *   - Constitutional Court (Bundesverfassungsgericht): Organized enforcer (organized/mobile) — Interprets and enforces Articles 1–19; coordinates the entire system; develops jurisprudence expanding rights reach
 *   - Federal Legislature: Institutional actor (institutional/mobile) — Cannot repeal Articles 1–19 due to eternity clause; legislates within rights framework; both benefits (legitimate authority) and constrained (proportionality review)
 *   - Vulnerable Populations (Under-Protected Groups): Primary beneficiary over generational time (powerless/constrained) — Rights catalog expands protections through constitutional court interpretation; solves collective action problem
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — Views rights catalog as coordination mechanism solving democratic constitutionalism's core problem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(german_basic_law__basic_rights_catalog, 0.28).
domain_priors:suppression_score(german_basic_law__basic_rights_catalog, 0.15).
domain_priors:theater_ratio(german_basic_law__basic_rights_catalog, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(german_basic_law__basic_rights_catalog, extractiveness, 0.28).
narrative_ontology:constraint_metric(german_basic_law__basic_rights_catalog, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(german_basic_law__basic_rights_catalog, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(german_basic_law__basic_rights_catalog, rope).
narrative_ontology:human_readable(german_basic_law__basic_rights_catalog, "German Basic Law: Rights Catalog (Articles 1–19)").
narrative_ontology:topic_domain(german_basic_law__basic_rights_catalog, "political/legal/constitutional").

domain_priors:requires_active_enforcement(german_basic_law__basic_rights_catalog).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(german_basic_law__basic_rights_catalog, '04998219-05fe-42df-843e-1f534a0635ee').
narrative_ontology:cs_kernel_codification('04998219-05fe-42df-843e-1f534a0635ee', fixed_text).
narrative_ontology:cs_authority_grounding('04998219-05fe-42df-843e-1f534a0635ee', lineage).
narrative_ontology:cs_interpretation_layer_present('04998219-05fe-42df-843e-1f534a0635ee').
narrative_ontology:cs_reading_relation('04998219-05fe-42df-843e-1f534a0635ee', german_basic_law__amendment_history, coexists_with).
narrative_ontology:cs_reading_relation('04998219-05fe-42df-843e-1f534a0635ee', german_basic_law__dignity_and_eternity, coexists_with).
narrative_ontology:cs_reading_relation('04998219-05fe-42df-843e-1f534a0635ee', german_basic_law__federal_construction, coexists_with).
narrative_ontology:cs_reading_relation('04998219-05fe-42df-843e-1f534a0635ee', german_basic_law__militant_democracy, coexists_with).
narrative_ontology:cs_axiom('04998219-05fe-42df-843e-1f534a0635ee', foundational, all_state_power_proportionality_bound).
narrative_ontology:cs_axiom_status(all_state_power_proportionality_bound, holdable).
narrative_ontology:cs_axiom_grounding('04998219-05fe-42df-843e-1f534a0635ee', all_state_power_proportionality_bound, deontological).
narrative_ontology:cs_axiom('04998219-05fe-42df-843e-1f534a0635ee', foundational, article_79_3_eternity_clause_enforcement).
narrative_ontology:cs_axiom_status(article_79_3_eternity_clause_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('04998219-05fe-42df-843e-1f534a0635ee', article_79_3_eternity_clause_enforcement, conventional).
narrative_ontology:cs_reference_frame('04998219-05fe-42df-843e-1f534a0635ee', proportional_constitutional_state).
narrative_ontology:cs_drift_state('04998219-05fe-42df-843e-1f534a0635ee', contemporary_post_2008_crisis, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('04998219-05fe-42df-843e-1f534a0635ee', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(german_basic_law__basic_rights_catalog, german_basic_law).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(german_basic_law__basic_rights_catalog, all_rights_holders).
narrative_ontology:constraint_beneficiary(german_basic_law__basic_rights_catalog, german_residents_and_citizens).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL RIGHTS-HOLDER (ROPE) — Articles 1–19 directly bind all public power and protect against state action. The rights catalog provides genuine coordination: it frames all administrative action as justiciable and proportional. No exit from the Basic Law's jurisdiction, but the constraint itself solves the coordination problem of unreviewable state power. The theatrical element is minimal — rights are regularly enforced through constitutional courts.
constraint_indexing:constraint_classification(german_basic_law__basic_rights_catalog, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: VULNERABLE POPULATION (ROPE) — Over generational time, the rights catalog has expanded protections through constitutional court interpretation (dignity jurisprudence, social welfare implications, labor rights). The constraint solves a collective action problem: without the binding catalog, vulnerable groups would face unreviewable extraction by legislative majorities. The mechanism is coordination, not extraction — the constraint enables mobilization of legal claims.
constraint_indexing:constraint_classification(german_basic_law__basic_rights_catalog, rope,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE ADMINISTRATION (TANGLED ROPE) — Articles 1–19 directly bind state organs and limit emergency powers. The administration experiences the rights catalog as both coordination and constraint. Beneficiary aspect: the catalog provides legitimate authority — administrative action grounded in constitutional rights justification gains public acceptance and reduces contestation. Victim aspect: the proportionality requirement imposes compliance costs and disallows purely extractive action. Active enforcement through constitutional court review is required to maintain the binding.
constraint_indexing:constraint_classification(german_basic_law__basic_rights_catalog, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL COURT (ROPE) — The court coordinates the entire system: interprets Article 1–19 protections, enforces them against state action, and develops jurisprudence expanding their reach. The court's power is substantial but exercised transparently and justifiably. The constraint is genuinely coordinative from this perspective — the court enables rights-holders and constrains arbitrary state power. No extraction; high legitimacy.
constraint_indexing:constraint_classification(german_basic_law__basic_rights_catalog, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: FEDERAL LEGISLATURE (TANGLED ROPE) — The legislature cannot repeal Articles 1–19 due to Article 79(3); it can only reinterpret or legislate within the rights framework. Beneficiary aspect: the rights catalog provides legitimate authority for legislation — laws grounded in constitutional rights justification command broader support. Victim aspect: the eternity clause (Article 79(3)) forecloses amendment of the essential content of any right; emergency powers are limited; legislative majorities cannot extract rights without proportionality review. Active enforcement through constitutional court review constrains the legislature's options.
constraint_indexing:constraint_classification(german_basic_law__basic_rights_catalog, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From civilizational scope, the Basic Law's rights catalog solves a core constitutional problem: how to protect individual liberty against majoritarian extraction and state power. The mechanism is pure coordination — it defines the terms under which state power is legitimate (proportional, justified by rights, subject to judicial review). The structure replicates across democracies; extractiveness is minimal because the constraint's function IS to prevent extraction. The theater ratio is low because rights enforcement is justiciable and transparent.
constraint_indexing:constraint_classification(german_basic_law__basic_rights_catalog, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(german_basic_law__basic_rights_catalog_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(german_basic_law__basic_rights_catalog, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(german_basic_law__basic_rights_catalog, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(german_basic_law__basic_rights_catalog_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The rights catalog's structural function is to constrain extraction by requiring state action to be justified by rights and subject to proportionality review. The metric captures that some extraction persists: (1) enforcement costs are borne by rights-holders seeking judicial remedy; (2) administrative action may be formally proportionate but substantively disadvantageous (e.g., regulations that comply with rights but burden specific groups); (3) the catalog itself can be used to justify action that technically respects rights but serves extractive purposes (e.g., security exceptions). However, the categorical binding of all state organs and the directly justiciable enforcement mechanism keep extractiveness low. Rising trajectory over 75 years (0.15 → 0.22 → 0.28) reflects increasing complexity of rights jurisprudence and expansion of rights scope through constitutional court interpretation — as the rights catalog's reach extends, compliance costs and interpretive friction increase. Suppression (0.15): Low. The rights catalog explicitly suppresses state power that lacks proportional justification. However, suppression is not total because: (1) state action can always be justified through proportionality testing; (2) emergency powers exist (Article 20(4) exception); (3) rights forfeiture is possible under Article 18. Theater ratio (0.35): Low-moderate. Rights enforcement is justiciable and rule-based, with transparent court proceedings and published jurisprudence. However, some theatrical elements persist: (1) proportionality balancing has indeterminate inputs and outputs; (2) constitutional courts use rhetoric and narrative framing in decisions; (3) emergency declarations can claim proportionality ex post. Rising trajectory (0.25 → 0.30 → 0.35) reflects increasing complexity of rights jurisprudence and the multiplication of balancing tests.
 *
 * PERSPECTIVAL GAP:
 *   The most significant perspectival gap appears between the individual rights-holder (rope, immediate/biographical) and the state administration (tangled_rope, biographical). From the individual's view, the rights catalog is pure coordination — it provides binding protection against extraction. From the state's view, the rights catalog imposes both coordination benefits (legitimate authority) and extraction costs (proportionality constraints). The gap reveals the constraint's hybrid function: it IS coordination (solving the problem of unreviewable state power) AND it IS constraint (limiting state extraction possibilities). At the civilizational/analytical level, the constraint appears as nearly pure rope because its function is transparent and its mechanism is coordinative. Over biographical time, the same constraint appears more extractive (tangled_rope) because administrative and legislative actors experience ongoing compliance burden and proportionality review. The federal legislature's perspective (tangled_rope, institutional) reveals the eternity clause's binding force — the legislature benefits from the rights catalog's legitimacy but cannot exit or amend it, creating a permanent constraint on sovereign power.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective experiences the rights catalog from a different structural position. The powerless rights-holder (constrained exit) experiences d ≈ 0.30 (beneficiary with high exit costs) — the rights catalog solves their collective action problem but provides no exit. The state administration (moderate, constrained) experiences d ≈ 0.55 (mixed beneficiary and victim) — benefits from legitimate authority but constrained by proportionality. The constitutional court (organized, mobile) experiences d ≈ 0.20 (powerful beneficiary with exit mobility) — coordinates the system and enforces its own readings. The legislature (institutional, mobile) experiences d ≈ 0.40 (powerful actor with eternity-clause constraints) — legislates within framework but cannot escape proportionality review. The analytical observer (analytical, analytical) experiences d ≈ 0.72 (neutral analytical position) — sees coordination function clearly. The directionality structure reveals that the beneficiary set (all rights-holders) is larger and more powerless than typical institutional constraints, while the victim set (extractive state power) is abstract and lacks direct agency. This asymmetry drives the rope classification — the constraint solves a coordination problem (how to bind state power proportionally) rather than extracting from a concentrated victim group.
 *
 * MANDATROPHY ANALYSIS:
 *   The basic_rights_catalog reading resolves the mandatrophy by anchoring to the proportionality mechanism: the rights catalog's structural function is to make ALL state power justiciable and proportional. This prevents the mandatrophy of confusing 'rights-protecting coordination' with 'extractive constraint on rights'. The constraint is NOT a mountain (natural law of justice) — it is contingent institutional arrangement that could be amended or abandoned. It IS a rope (genuine coordination solving the problem of unreviewable power) because its mechanism is transparent, justiciable, and serves all participants' interests in legitimate state action. However, the tangled_rope perspectives from the state administration and legislature reveal that the constraint DOES impose costs (proportionality compliance, limited amendment power). These costs are not extraction in the predatory sense — they are coordination costs of living in a legitimate constitutional order. The mandatrophy resolves when we recognize that a constraint can be simultaneously coordinative (solving a collective action problem) and limitative (imposing costs on state power). The Basic Law's rights catalog is both. The theater ratio (0.35) is low enough to confirm that the constraint is not primarily performative — rights enforcement is real and consequential. The extractiveness (0.28) is low enough to confirm that the constraint's function is coordination, not extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_79_3_amendment_scope,
    'Does the ''essence'' clause in Article 79(3) bar ONLY amendment of Articles 1–19, or does it protect the entire constitutional structure including federalism and democratic process?',
    'Textual analysis of Article 79(3): ''The substance of Articles 1–20'' vs interpretive extension to the entire constitutional order. Bundesverfassungsgericht jurisprudence on the limits of amendment.',
    'Narrow reading: Basic Law can be substantially reformed by supermajority (Amendment history reading gains force). Broad reading: the constitutional core is untouchable, and the dignity-and-eternity reading gains force. Different constraint boundaries for each reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_79_3_amendment_scope, conceptual, 'Scope of the Article 79(3) unamendable core').

omega_variable(
    horizontal_effect_of_rights,
    'Do Articles 1–19 bind private actors (horizontal effect), or only state organs (vertical effect)?',
    'Constitutional court jurisprudence on Drittwirkung (third-party effect); case law distinguishing immediate horizontal effect (rare) from indirect horizontal effect through statutory interpretation and public law doctrine.',
    'Vertical effect only: rights-holders have no protection against private extraction (employer, landlord, platform). Horizontal effect: beneficiary set includes protection from private power; extractiveness and suppression metrics shift upward; scope expands beyond state administration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(horizontal_effect_of_rights, empirical, 'Whether fundamental rights bind private actors').

omega_variable(
    proportionality_as_extraction_filter,
    'Is proportionality review a genuine limit on extraction, or a post-hoc justification mechanism that state power uses to rationalize extractive action?',
    'Empirical analysis of constitutional court invalidation rates for state action on rights grounds; comparison of proportionality outcomes across demographic groups (privileged vs vulnerable). Historical record of emergency measures deemed ''proportionate''.',
    'If proportionality is genuine limit: extractiveness remains ~0.28 (rope category). If proportionality is rationalization: extractiveness rises to 0.45–0.55 (tangled rope). Classification shifts based on whether the judicial constraint is effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_as_extraction_filter, empirical, 'Whether proportionality review effectively constrains state extraction').

omega_variable(
    reading_vs_sibling_foreclosure,
    'This reading (basic_rights_catalog) emphasizes the horizontal binding of state organs by Articles 1–19. Does this reading FORECLOSE the dignity_and_eternity reading (which emphasizes Article 1''s inviolability and unamendability), or do both readings coexist in a single constitutional framework?',
    'Constitutional theory: Can both readings be held simultaneously within German constitutional law? Does emphasizing the rights catalog preclude emphasizing dignity as the inviolable foundation? Or are they complementary axioms of the same order?',
    'If foreclosed: basic_rights_catalog and dignity_and_eternity are mutually exclusive constraints (different files, different ε values). If coexist: both readings are live positions within German constitutionalism, and the kernel exhibits internal pluralism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_sibling_foreclosure, conceptual, 'Whether this reading forecloses the dignity_and_eternity reading').

omega_variable(
    militant_democracy_rights_forfeiture,
    'Does Article 21(2) (party ban) or Article 18 (rights forfeiture) constitute an exception to the ''untouchable essence'' of Articles 1–19, or does the basic_rights_catalog reading interpret these as consistent with constitutional rights because they protect the constitutional order itself?',
    'Constitutional court jurisprudence on Article 21(2) and 18: Is rights forfeiture a rights violation, or is protecting the constitution a grounds for limiting rights? Can democracy defend itself through anti-democratic means while remaining rights-bound?',
    'If exception: the rights catalog has articulated limits, and the militant_democracy reading creates a structural pressure that constrains basic_rights_catalog scope. If consistent: the basic_rights_catalog reading absorbs militant democracy as a secondary principle, and no sibling constraint pressure emerges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militant_democracy_rights_forfeiture, conceptual, 'Whether militant democracy''s rights forfeiture fits within the rights catalog').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(german_basic_law__basic_rights_catalog, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gbl_rights_theater_1949, german_basic_law__basic_rights_catalog, theater_ratio, 0, 0.25).
narrative_ontology:measurement(gbl_rights_theater_1984, german_basic_law__basic_rights_catalog, theater_ratio, 35, 0.3).
narrative_ontology:measurement(gbl_rights_theater_2024, german_basic_law__basic_rights_catalog, theater_ratio, 75, 0.35).

% Extraction over time
narrative_ontology:measurement(gbl_rights_extract_1949, german_basic_law__basic_rights_catalog, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(gbl_rights_extract_1984, german_basic_law__basic_rights_catalog, base_extractiveness, 35, 0.22).
narrative_ontology:measurement(gbl_rights_extract_2024, german_basic_law__basic_rights_catalog, base_extractiveness, 75, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(german_basic_law__basic_rights_catalog, enforcement_mechanism).
narrative_ontology:affects_constraint(german_basic_law__basic_rights_catalog, german_basic_law__dignity_and_eternity).
narrative_ontology:affects_constraint(german_basic_law__basic_rights_catalog, german_basic_law__amendment_history).
narrative_ontology:affects_constraint(german_basic_law__basic_rights_catalog, german_basic_law__federal_construction).
narrative_ontology:affects_constraint(german_basic_law__basic_rights_catalog, german_basic_law__militant_democracy).

% DUAL FORMULATION NOTE:
% The basic_rights_catalog reading is one of five distinct interpretations of the German Basic Law kernel. Each reading emphasizes different structural features (rights catalog vs dignity foundation vs federalism vs democratic defense) and produces different epsilon values. All five readings are live positions within German constitutional discourse; no single reading foreclosed by another (they coexist). However, the basic_rights_catalog reading INFLUENCES the dignity_and_eternity and militant_democracy readings by establishing the proportionality framework within which those readings operate. The amendment_history and federal_construction readings operate at different structural levels and do not directly interact with this reading's classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
