% ============================================================================
% CONSTRAINT STORY: free_speech_clause__categorical_exceptions_doctrine
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_free_speech_clause__categorical_exceptions_doctrine, []).

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
 *   constraint_id: free_speech_clause__categorical_exceptions_doctrine
 *   human_readable: Free Speech Clause: Categorical Exceptions Doctrine
 *   domain: legal/doctrinal
 *
 * SUMMARY:
 *   The categorical exceptions doctrine is one reading of how the First
 *   Amendment constrains government speech regulation. This reading holds
 *   that certain categories of speech — incitement, obscenity, defamation,
 *   true threats, and fighting words — fall outside constitutional protection
 *   *by definition*. Once a utterance fits a category, suppression is
 *   permitted without content-based scrutiny. This constraint exhibits
 *   tangled-rope structure: it coordinates speech regulation through clear
 *   categorical boundaries (benefiting speakers outside the categories) while
 *   extracting from speakers whose speech fits the categories (the victims).
 *   The categorical doctrine creates a dual regime: protected speech receives
 *   no suppression; categorical speech receives automatic suppression. The
 *   constraint's extractiveness (0.38) reflects moderate asymmetry — the
 *   categories are specific enough to provide some predictability, but their
 *   application is contested (especially for obscenity, incitement, and true
 *   threats), creating suppression (0.52) through legal uncertainty. The
 *   theater ratio (0.35) is relatively low because categorical application is
 *   genuinely doctrinal — courts apply specific tests (Brandenburg for
 *   incitement, Miller for obscenity) rather than performing pure ritual.
 *   However, the theater is rising (0.25→0.35 over 50 years) as technological
 *   change has outpaced the categories' original design. This constraint is
 *   ONE READING of the free_speech_clause kernel. Other readings
 *   (content_neutrality_doctrine, public_forum_doctrine) offer different
 *   structural logics for the same constitutional text. The categorical
 *   reading is the oldest (common law origins in Chaplinsky 1942) and remains
 *   the formal rule, but it is being pressed by rival readings and by
 *   technological harms that fit no category.
 *
 * KEY AGENTS:
 *   - Speakers Outside Enumerated Categories: Primary beneficiary (institutional/arbitrage) — receives unconditional protection; knows the law is clear
 *   - Speakers Within Enumerated Categories: Primary victim (powerless/trapped) — loses protection by categorical membership; no escape route
 *   - Harms Outside Categories: Secondary victim (powerless/trapped) — victims of speech that causes real harm but fits no exception; harm is unconstrained by law
 *   - Judicial System: Institutional enforcer (institutional/arbitrage) — applies categorical tests through established doctrine; maintains the categorical frame through repeated application
 *   - Civil Rights Litigation Sector: Organized challenger (organized/constrained) — generates test cases that push category boundaries; can reshape doctrine over generational time
 *   - Congressional Actors: Institutional potential modifier (institutional/arbitrage) — could create new categories via statute (rare; FOSTA-SESTA as example) or could amend the Constitution (unprecedented for free speech exceptions)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the categorical boundaries as immutable features of speech itself rather than contingent doctrinal constructions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(free_speech_clause__categorical_exceptions_doctrine, 0.38).
domain_priors:suppression_score(free_speech_clause__categorical_exceptions_doctrine, 0.52).
domain_priors:theater_ratio(free_speech_clause__categorical_exceptions_doctrine, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(free_speech_clause__categorical_exceptions_doctrine, extractiveness, 0.38).
narrative_ontology:constraint_metric(free_speech_clause__categorical_exceptions_doctrine, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(free_speech_clause__categorical_exceptions_doctrine, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(free_speech_clause__categorical_exceptions_doctrine, tangled_rope).
narrative_ontology:human_readable(free_speech_clause__categorical_exceptions_doctrine, "Free Speech Clause: Categorical Exceptions Doctrine").
narrative_ontology:topic_domain(free_speech_clause__categorical_exceptions_doctrine, "legal/doctrinal").

domain_priors:requires_active_enforcement(free_speech_clause__categorical_exceptions_doctrine).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(free_speech_clause__categorical_exceptions_doctrine, '9c0d1203-754a-4513-8790-0dca40d22915').
narrative_ontology:cs_kernel_codification('9c0d1203-754a-4513-8790-0dca40d22915', formalized).
narrative_ontology:cs_authority_grounding('9c0d1203-754a-4513-8790-0dca40d22915', lineage).
narrative_ontology:cs_interpretation_layer_present('9c0d1203-754a-4513-8790-0dca40d22915').
narrative_ontology:cs_reading_relation('9c0d1203-754a-4513-8790-0dca40d22915', free_speech_clause__content_neutrality_doctrine, coexists_with).
narrative_ontology:cs_reading_relation('9c0d1203-754a-4513-8790-0dca40d22915', free_speech_clause__public_forum_doctrine, influences).
narrative_ontology:cs_axiom('9c0d1203-754a-4513-8790-0dca40d22915', foundational, enumerated_categories_exhaustive).
narrative_ontology:cs_axiom_status(enumerated_categories_exhaustive, holdable).
narrative_ontology:cs_axiom_grounding('9c0d1203-754a-4513-8790-0dca40d22915', enumerated_categories_exhaustive, conventional).
narrative_ontology:cs_axiom('9c0d1203-754a-4513-8790-0dca40d22915', foundational, categorical_membership_determinable).
narrative_ontology:cs_axiom_status(categorical_membership_determinable, holdable).
narrative_ontology:cs_axiom_grounding('9c0d1203-754a-4513-8790-0dca40d22915', categorical_membership_determinable, empirically_contingent).
narrative_ontology:cs_reference_frame('9c0d1203-754a-4513-8790-0dca40d22915', chaplinsky_categorical_baseline).
narrative_ontology:cs_drift_state('9c0d1203-754a-4513-8790-0dca40d22915', contemporary_internet_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9c0d1203-754a-4513-8790-0dca40d22915', '2026-02-26T14:23:45Z').
narrative_ontology:cs_kernel_id(free_speech_clause__categorical_exceptions_doctrine, free_speech_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(free_speech_clause__categorical_exceptions_doctrine, speakers_outside_enumerated_categories).
narrative_ontology:constraint_victim(free_speech_clause__categorical_exceptions_doctrine, speakers_within_categories).
narrative_ontology:constraint_victim(free_speech_clause__categorical_exceptions_doctrine, harms_unconstrained_by_categories).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INCITEMENT DEFENDANT (SNARE) — Powerless speaker trapped within the enumerated category 'incitement' by judicial interpretation of Brandenburg standard. No exit option; category membership is structurally determined. High experienced extraction: loses protection *by definition*, not by content analysis. The category confines the victim's options and forecloses remedy.
constraint_indexing:constraint_classification(free_speech_clause__categorical_exceptions_doctrine, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: OBSCENITY PUBLISHER (TANGLED ROPE) — Moderate power speaker constrained by the enumerated category 'obscenity' but able to contest category membership (Miller test, contemporary community standards). Some coordination benefit exists: the category clarifies what is outside protection, enabling surrounding speakers to navigate the law. Some extraction: the category's vagueness (community standards) creates suppression. Constrained exit — can try to reframe as non-obscene, at cost.
constraint_indexing:constraint_classification(free_speech_clause__categorical_exceptions_doctrine, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MAINSTREAM MEDIA CORPORATION (ROPE) — Institutional power speaker with arbitrage options. Benefits from clear categorical boundaries: knows what is protected (news, commentary, fiction) and what is not (incitement, defamation). The enumerated categories function as coordination mechanism, not extraction. Can navigate by genre and context. Low experienced extraction because categories are predictable and the speaker has resources to work within them.
constraint_indexing:constraint_classification(free_speech_clause__categorical_exceptions_doctrine, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CIVIL RIGHTS MOVEMENT (TANGLED ROPE) — Organized agents (ACLU, plaintiffs, activist groups) see categorical doctrine as both enabling (clear rules to challenge) and constraining (categories must be stretched or reinterpreted to protect new forms of speech). Generate-and-test strategy: each new category gets questioned (was student armband-wearing incitement? Brandenburg 1969; was nude dancing speech? Barnes 1991). Moderate extraction because the organized sector can contest and reshape categories over time, but also coordinated benefit from knowing the rule structure. Generational time horizon because category expansion takes decades of litigation.
constraint_indexing:constraint_classification(free_speech_clause__categorical_exceptions_doctrine, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: HARMS OUTSIDE ENUMERATED CATEGORIES (SNARE) — Victims of speech that causes harm but fits no enumerated exception (e.g., algorithmic recommendation amplification, harassment via repeated lawful speech, coordinated inauthentic behavior, deepfake manipulation) are trapped. The categorical structure protects speakers outside the categories unconditionally. Victims trapped in generational time because creating new categories requires constitutional amendment or Supreme Court reversal of settled doctrine. Pure extraction from the victim's perspective: harm is real but unconstrained by law.
constraint_indexing:constraint_classification(free_speech_clause__categorical_exceptions_doctrine, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 6: JUDICIAL ENFORCEMENT SYSTEM (PITON) — Courts apply categorical doctrine through ritual: Brandenburg test for incitement, Miller test for obscenity, defamation elements, true threats analysis, fighting words context. The ritual persists as the standard form even though technological change (internet speech, algorithms, synthetic media) has outpaced the categorical frame. The system maintains theatrical application of historical categories to novel harms. Piton classification: high theater (0.35 is deceptively low — the Miller test itself is nearly 50 years old and applied to digital images it was never designed for), degraded function (categories no longer map cleanly to technological speech forms), inertial maintenance (courts continue because the doctrine is settled, not because it works).
constraint_indexing:constraint_classification(free_speech_clause__categorical_exceptions_doctrine, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some speech causes such clear and direct harm (incitement to imminent lawless action, true threats of immediate violence) that protection must be withheld. This perspective sees the categorical exceptions as discovering immutable boundaries, not constructing them. However, the structural data reveals this as a false summit: the beneficiaries are identifiable (speakers outside categories), victims exist (trapped speakers within categories), and enforcement is active (courts apply tests). The 'natural' categories are institutional artifacts maintained by a specific doctrine, not laws of nature.
constraint_indexing:constraint_classification(free_speech_clause__categorical_exceptions_doctrine, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(free_speech_clause__categorical_exceptions_doctrine_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(free_speech_clause__categorical_exceptions_doctrine, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(free_speech_clause__categorical_exceptions_doctrine, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(free_speech_clause__categorical_exceptions_doctrine, TR),
    TR >= 0.70.

:- end_tests(free_speech_clause__categorical_exceptions_doctrine_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The categorical doctrine creates moderate asymmetry between speakers inside and outside the enumerated categories. Speakers outside categories receive robust protection; speakers inside categories lose protection by definition. However, the extraction is not severe (not >0.46) because: (1) category membership is theoretically contestable through litigation (Brandenburg test, Miller test) — speakers have some exit option through appellate challenge; (2) the categories are historically specific (incitement, obscenity) rather than open-ended (content-based suppression), which limits category creep; (3) enforcement is procedurally bounded by constitutional review. If the categories were expandable at will, or if membership determination were purely subjective, extractiveness would be higher. Suppression (0.52): Moderate-high. Speakers within categories face significant suppression: legal barriers to publication, criminal penalties, civil liability. But suppression is not total (not ≥0.60 for snare threshold) because: (1) the categories are specific — not all speech is within them; (2) some speakers can navigate by reframing (e.g., sexual speech as artistic rather than obscene); (3) some categories have narrow triggers (Brandenburg requires imminence, true threats require specificity). Theater ratio (0.35): Low-moderate. The categorical doctrine is genuinely doctrinal — courts apply specific, named tests rather than performing pure theatrical review. However, theater is present and rising because: (1) the tests themselves (Miller's three-prong obscenity test, Brandenburg's imminence standard) were designed for mid-20th century speech forms and are applied to internet speech, synthetic media, and algorithmic amplification they were never designed to address; (2) courts continue to apply categorical review to novel harms that fit no category, creating a kind of theatrical gap-filling. The rising theater_ratio (0.25→0.35) reflects increasing mismatch between doctrinal categories and technological speech forms.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates significant perspectival divergence. The speaker outside categories (institutional, arbitrage) sees pure coordination (Rope) — the categorical structure clarifies what is protected, enabling confident speech. The mainstream media corporation experiences only low extraction because it has resources to navigate categorical boundaries. The speaker inside a category (powerless, trapped) sees snare — the category confines them by definition, with no exit. The harms outside categories (also snare) perceive unconstrained harm — the categorical structure protects speakers but leaves victims unprotected. The organized litigators (ACLU, civil rights groups) see tangled-rope because they can contest categories over generational time but operate under constraints (resource limits, appellate deference to doctrine). The courts themselves see piton — they maintain the categorical review ritual even as it becomes increasingly theatrical. The analytical observer at civilizational scale risks seeing mountain (categorical boundaries as natural laws of speech itself), but the structural data reveals this as a false summit: the categories are historically contingent (Chaplinsky, 1942), beneficiaries are identifiable (speakers outside categories), and the doctrine is actively enforced.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by each agent's structural position relative to this constraint. Beneficiaries (speakers outside categories) with arbitrage options get low d (~0.15) — they can easily exit the constraint's suppressive effects by speaking outside categories. Victims within categories (trapped speakers) get high d (~0.85) — they cannot exit; their speech by definition fits a suppressed category. Victims whose harms are unconstrained (harms outside categories) also get high d (~0.85) — they cannot exit; the constraint actively protects the speakers who harm them. The judicial system (institutional, arbitrage) gets low d because it can reframe, reinterpret, and reshape categorical doctrine over time (arbitrage). The organized litigators (organized, constrained) get moderate d (~0.55) because they can contest categories but face resource and precedential constraints. The analytical observer (analytical) gets the canonical d for the analytical power atom (~0.72), reflecting that the observer is not structurally embedded in the constraint but can model it.
 *
 * MANDATROPHY ANALYSIS:
 *   READING-SPECIFIC MANDATROPHY: This constraint exemplifies how a single constitutional kernel (free_speech_clause) can instantiate different mandatrophies depending on which reading is adopted. Under categorical_exceptions_doctrine: the mandatrophy is whether enumerated categories are exhaustive and immutable, or whether new harms can create new exceptions. Under content_neutrality_doctrine: the mandatrophy is whether the rule is content-based or content-neutral, shifting the analytical focus from category membership to regulatory intent. Under public_forum_doctrine: the mandatrophy is about geographic forum classification, not categorical membership at all. Each reading resolves the tension between free speech and legitimate harm-prevention differently. The categorical reading concentrates suppression on specific, historic categories — it trades off coverage (doesn't protect all speech) for clarity (speakers know what is suppressed). The content-neutrality reading trades off specificity for process-neutrality (any category could be valid if neutrally applied). The public-forum reading trades off both for geography (speech in streets is maximally protected regardless of category). The engine does not declare one reading as 'correct' — the mandatrophy is resolved by making explicit which reading you are measuring from. This constraint instantiates categorical_exceptions_doctrine, and from that reading's perspective, the mandatrophy is resolved: yes, categorized speech can be suppressed without content-based review, and this is legitimate because the categories are narrowly drawn and historically specific.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    category_membership_determination,
    'Who determines membership in enumerated categories? Is the determination objective (Brandenburg imminent lawlessness test) or subjective (Miller contemporary community standards)?',
    'Empirical analysis of appellate reversal rates; comparison of category application consistency across jurisdictions and time periods',
    'If objective: categories function as true coordination boundaries. If subjective: categories become instruments of selective suppression, shifting classification from tangled_rope toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(category_membership_determination, empirical, 'Objectivity of categorical membership determination').

omega_variable(
    new_category_creation_resistance,
    'Can the categorical doctrine be extended to new harms (algorithmic amplification, coordinated inauthentic behavior, deepfakes), or does the doctrine''s closed set of historic categories resist expansion?',
    'Case law tracking: whether courts propose new categorical exceptions or refuse to extend existing ones; whether Congress attempts statutory extension (e.g., FOSTA-SESTA as category expansion for online facilitation)',
    'If extendable: doctrine can accommodate new harms, limiting victim-trapping at the snare perspective. If closed: harms outside categories remain unconstrained, and the snare perspective''s extraction intensifies over time as technology creates novel harm vectors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(new_category_creation_resistance, empirical, 'Whether categorical doctrine resists or accepts new exceptions').

omega_variable(
    reading_vs_content_neutrality_precedence,
    'When categorical exceptions doctrine conflicts with content-neutrality doctrine (e.g., can government ban a category of speech based on content when the category is enumerated?), which reading controls?',
    'Supreme Court doctrine clarification; analysis of how content-neutrality cases treat categorical exceptions (e.g., does strict scrutiny apply to categorical restrictions?)',
    'If categorical doctrine controls: categories can suppress content by definition, foreclosing content-neutrality reading. If content-neutrality controls: categorical exceptions themselves are subject to content-neutrality review, influencing and constraining this reading. Current doctrine appears to coexist, but the relationship is under-theorized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_content_neutrality_precedence, conceptual, 'Precedence relationship between categorical and content-neutrality doctrines').

omega_variable(
    kernel_reading_ambiguity,
    'Is the categorical exceptions doctrine a *reading* of the First Amendment''s meaning (one interpretation among others) or the *substance* of the constitutional rule itself?',
    'Historical jurisprudence analysis: trace whether the categories predate constitutional interpretation (common law origins) or were constructed by modern courts (Chaplinsky 1942 onward)',
    'If reading: this constraint is subject to the oracle gap — other readings (content-neutrality, public-forum) may be equally valid. If substance: this constraint defines the constitutional rule and foreclosed other readings. The committer frame assumes reading status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether categorical exceptions doctrine is constitutional reading or substance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(free_speech_clause__categorical_exceptions_doctrine, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fsc_cat_tr_t0, free_speech_clause__categorical_exceptions_doctrine, theater_ratio, 0, 0.25).
narrative_ontology:measurement(fsc_cat_tr_t25, free_speech_clause__categorical_exceptions_doctrine, theater_ratio, 25, 0.3).
narrative_ontology:measurement(fsc_cat_tr_t50, free_speech_clause__categorical_exceptions_doctrine, theater_ratio, 50, 0.35).

% Extraction over time
narrative_ontology:measurement(fsc_cat_be_t0, free_speech_clause__categorical_exceptions_doctrine, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(fsc_cat_be_t25, free_speech_clause__categorical_exceptions_doctrine, base_extractiveness, 25, 0.33).
narrative_ontology:measurement(fsc_cat_be_t50, free_speech_clause__categorical_exceptions_doctrine, base_extractiveness, 50, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(free_speech_clause__categorical_exceptions_doctrine, enforcement_mechanism).
narrative_ontology:affects_constraint(free_speech_clause__categorical_exceptions_doctrine, free_speech_clause__content_neutrality_doctrine).
narrative_ontology:affects_constraint(free_speech_clause__categorical_exceptions_doctrine, free_speech_clause__public_forum_doctrine).

% DUAL FORMULATION NOTE:
% The free_speech_clause kernel decomposes into three structurally distinct constraint readings: categorical_exceptions_doctrine (this story), content_neutrality_doctrine (sibling), and public_forum_doctrine (sibling). Each reading offers a different ε-invariant interpretation of how the First Amendment constrains speech regulation. The categorical reading has ε≈0.38 and structures suppression around enumerated category membership. The content-neutrality reading has different ε (likely ≈0.42) and structures suppression around regulatory intent (content-based vs. content-neutral). The public-forum reading has different ε (likely ≈0.35) and structures suppression around geographic forum classification. All three are live readings in contemporary case law; none fully forecloses the others (though they create pressure on each other).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
