% ============================================================================
% CONSTRAINT STORY: substitution_archive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substitution_archive, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: substitution_archive
 *   human_readable: Kodashim as Substitution Archive: Prayer and Study Replace Sacrifice
 *   domain: religious_studies/rabbinic_judaism/commitment_systems
 *
 * SUMMARY:
 *   In Kodashim, the Mishnaic tractates documenting the laws and procedures
 *   of Temple sacrifice, Rabbinic Judaism created an archive—or a memorial,
 *   or a permanent substitute, depending on which reading occupies the
 *   kernel. This constraint story instantiates the 'substitution_archive'
 *   reading: the reading that holds Kodashim functions as a substitute for
 *   sacrificial practice that is claimed as continuation rather than
 *   replacement, obscuring extraction from those seeking actual restoration.
 *   Prayer and focused textual study replaced blood sacrifice and priestly
 *   ritual after the Temple's destruction in 70 CE, but the substitution was
 *   never framed as permanent abolition—it was framed as contingent deferral
 *   pending restoration. Kodashim preserves the knowledge and law as if
 *   waiting. But the waiting has become permanent through institutional
 *   authority's claim that study IS the equivalent, IS the continuation, IS
 *   sufficient. This constraint exhibits the core dynamics of a tangled rope:
 *   genuine coordination function (preserving detailed knowledge, enabling
 *   intellectual continuity), asymmetric extraction (denying restoration as
 *   legitimate aspiration or even possible), and active enforcement through
 *   institutional authority (diaspora rabbinic leadership maintains the
 *   substitution as normative and sufficient).
 *
 * KEY AGENTS:
 *   - Rabbinic Text-Study Institutions: Institutional beneficiary (institutional/arbitrage) — Kodashim provides status, interpretive authority, and legitimating corpus for diaspora Jewish leadership; benefits from claim that study substitutes for sacrifice.
 *   - Restoration-Seeking Practitioners: Primary victims (powerless/trapped) — told that their aspiration to restore Temple sacrifice is obsolete; extraction lies in being denied recognition that restoration is a live option within the legal framework.
 *   - Active Temple Restoration Movements: Secondary victim (moderate/constrained) — constrained by institutional barriers and by Kodashim's claim that restoration is unnecessary; some coordination benefit from detailed knowledge but extraction from delegitimization.
 *   - Diaspora Jewish Communities: Beneficiary (institutional/arbitrage) — the substitution enables diaspora survival without priesthood or temple; benefits from institutional framing that study maintains Jewish practice.
 *   - Jewish Renewal and Literal Reading Communities: Organized agents (organized/constrained) — see Kodashim as both preservation mechanism and barrier to alternative interpretations; some agency in how they study and frame the texts.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a contingent institutional substitution as structural inevitability inherent to diaspora Judaism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substitution_archive, 0.52).
domain_priors:suppression_score(substitution_archive, 0.58).
domain_priors:theater_ratio(substitution_archive, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substitution_archive, extractiveness, 0.52).
narrative_ontology:constraint_metric(substitution_archive, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(substitution_archive, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substitution_archive, tangled_rope).
narrative_ontology:human_readable(substitution_archive, "Kodashim as Substitution Archive: Prayer and Study Replace Sacrifice").
narrative_ontology:topic_domain(substitution_archive, "religious_studies/rabbinic_judaism/commitment_systems").

domain_priors:requires_active_enforcement(substitution_archive).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(substitution_archive, formalized).
narrative_ontology:cs_authority_grounding(substitution_archive, lineage).
narrative_ontology:cs_interpretation_layer_present(substitution_archive).
narrative_ontology:cs_kernel_id(substitution_archive, kodashim_corpus).
narrative_ontology:cs_reading_relation(substitution_archive, study_as_exercise, influences).
narrative_ontology:cs_reading_relation(substitution_archive, performance_only, coexists_with).
narrative_ontology:cs_axiom(substitution_archive, foundational, substitution_is_complete_and_sufficient).
narrative_ontology:cs_axiom_status(substitution_is_complete_and_sufficient, holdable).
narrative_ontology:cs_axiom_grounding(substitution_archive, substitution_is_complete_and_sufficient, deontological).
narrative_ontology:cs_axiom(substitution_archive, foundational, archive_preserves_actionable_law).
narrative_ontology:cs_axiom_status(archive_preserves_actionable_law, holdable).
narrative_ontology:cs_axiom_grounding(substitution_archive, archive_preserves_actionable_law, conventional).
narrative_ontology:cs_reference_frame(substitution_archive, substitution_permanent_and_normative).
narrative_ontology:cs_drift_state(substitution_archive, contemporary_diaspora_judaism, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substitution_archive, rabbinic_text_study_institutions).
narrative_ontology:constraint_beneficiary(substitution_archive, diaspora_jewish_communities).
narrative_ontology:constraint_victim(substitution_archive, restoration_seeking_practitioners).
narrative_ontology:constraint_victim(substitution_archive, sacrificial_restoration_movements).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESTORATION-SEEKING PRACTITIONER (SNARE) — Trapped by the claim that Kodashim study equals sacrifice restoration. Cannot exit the interpretive frame that reading Kodashim IS the legitimate continuation of Temple practice. Bears the extraction of deferred restoration; told the memorial archive is the living practice.
constraint_indexing:constraint_classification(substitution_archive, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ACTIVE RESTORATION MOVEMENT (TANGLED ROPE) — Constrained by institutional and political barriers to rebuilding; also constrained by the Kodashim archive's claim that study substitutes for practice. Some coordination function exists (Kodashim provides detailed knowledge of sacrificial procedures); significant extraction (denial that restoration is possible or necessary). Movement has agency but faces resource and legitimacy barriers.
constraint_indexing:constraint_classification(substitution_archive, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RABBINIC TEXT-STUDY INSTITUTIONS (ROPE) — Primary beneficiary with arbitrage options. Kodashim study generates status, interpretive authority, and institutional continuity. The constraint is experienced as pure coordination: documenting what was, preserving knowledge, enabling intellectual engagement. Net beneficiary; can exit to other texts.
constraint_indexing:constraint_classification(substitution_archive, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DIASPORA JEWISH AUTHORITY (PITON) — Maintains Kodashim as memorial archive through institutional inertia. The performative function (study as remembrance) persists; the coordination function (preserving sacrificial knowledge for possible restoration) has atrophied into theater. The authority structure sees Kodashim study as degraded but continues it because replacing the entire educational corpus would require dismantling diaspora rabbinic authority itself.
constraint_indexing:constraint_classification(substitution_archive, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: JEWISH RENEWAL COMMUNITIES (TANGLED ROPE) — Organized agents (reconstructionist movements, some Orthodox groups, post-denominational communities) experience Kodashim as coordination (detailed knowledge transmission) mixed with extraction (institutional pressure to treat study as sufficient substitute). These communities have agency and interpretive alternatives; constrained by diaspora norms and institutional legacy.
constraint_indexing:constraint_classification(substitution_archive, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL SUBSTITUTION (MOUNTAIN) — From a civilizational perspective, the substitution of study for sacrifice appears structurally inevitable: destroyed temples cannot function; study preserves knowledge; diaspora survival requires textual continuity. The constraint appears as a natural adaptation to irreversible historical conditions. However, the base properties reveal this as false-summit naturalization: the substitution is enforced through institutional authority, not as a result of physical law.
constraint_indexing:constraint_classification(substitution_archive, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substitution_archive_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(substitution_archive, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(substitution_archive, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(substitution_archive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(substitution_archive, TR),
    TR >= 0.70.

:- end_tests(substitution_archive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts from those seeking restoration by claiming substitution is complete and sufficient. The extraction is not maximal (snare-level 0.66+) because the coordination function is genuine—Kodashim does preserve knowledge, does enable intellectual engagement, does provide real coordination for diaspora Jewish practice. But the extraction is substantial because the claimed equivalence denies that restoration is a live option. Suppression (0.58): Moderate-high. Barriers to restoration include institutional pressure to treat study as sufficient, legitimacy costs of challenging the substitution, resource barriers, and political barriers. But suppression is not extreme because some communities maintain restoration movements and some authorities acknowledge the theoretical possibility. Theater ratio (0.68): High and increasing. Early rabbinic period: theater was lower (0.42) because the substitution was fresher, more obviously contingent, and more clearly framed as deferral. Middle period (Geonic/Rishonim): theater increased (0.58) as rabbinic authorities elaborated Kodashim study into a high-status intellectual practice. Contemporary diaspora: theater is highest (0.68) because the performative function (study as remembrance) has largely replaced the coordination function (preservation against restoration). The increasing theater ratio reflects the constraint's decay from coordination mechanism to institutional theater.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap lies between the beneficiary (rabbinic institutions) and the victim (restoration seekers). The beneficiary experiences Kodashim as coordination—transmitting knowledge, enabling interpretation, maintaining Jewish practice. The victim experiences Kodashim as extraction—being told their aspiration is obsolete, being denied recognition that restoration is possible. The piton perspective (diaspora authority structure) sees the constraint as degraded—the performative function persists but the coordination function has atrophied. The analytical observer risks seeing natural law (substitution is inevitable given destroyed temples) but the base properties reveal false summit: the substitution is enforced through institutional authority, not as a necessity of physical or social law. The organized renewal communities see mixed coordination (knowledge preservation) and extraction (denial of alternatives), producing the tangled rope classification. This gap is not resolvable by choosing the 'correct' perspective—it IS the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality d derives from the agent's structural position relative to the extraction flow. Rabbinic institutions are beneficiaries with arbitrage options (can exit to other texts, other interpretive practices)—low d. Restoration seekers are victims with no escape from the claim that substitution is complete—high d. Text-study communities are beneficiaries constrained by diaspora norms—moderate d. The analytical observer faces the risk of naturalizing contingent institutional arrangements (false summit)—d computed from the observer's structural relationship to the constraint, not from the observer's epistemic position. The directionality values feed into the sigmoid function f(d) to produce effective extractiveness χ for each perspective. This is why the beneficiary sees low χ (rope), the victim sees high χ (snare), and the organized agent sees mixed experience (tangled rope).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is genuinely tangled rope: it coordinates the preservation of sacrificial knowledge (real coordination function) while extracting through the claim that study is sufficient (asymmetric extraction denying restoration as legitimate). The false summit (analytical observer's mountain) is diagnostic of how institutional authority can naturalize contingent substitutions. The snare (restoration seeker's experience) is not the 'true' classification—it is the experienced reality from one structural position. The rope (beneficiary's experience) is genuine coordination, not denial of extraction. The piton (degraded ritual preservation) is real institutional inertia. No single type explains the constraint; the presheaf of perspectives does. Mandatrophy resolution requires holding the coordination and extraction functions simultaneously: yes, Kodashim preserves knowledge (coordination); yes, the claim that study substitutes for sacrifice denies restoration as legitimate (extraction). The constraint is both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substitution_vs_deferral_semantics,
    'Does Kodashim function as a completed substitution (prayer and study ARE the legitimate continuation of sacrifice) or as a deferral mechanism (study preserves knowledge UNTIL restoration becomes possible)?',
    'Textual analysis of Kodashim commentary across generations: do authorities treat the substitution as permanent or provisional? Comparative analysis of how Kodashim is framed relative to other displaced practices (e.g., Temple incense, priestly service). Investigation of whether restoration movements cite Kodashim as enabler or obstacle.',
    'If substitution is complete/permanent: constraint is pure coordination/rope + piton (text preservation). If deferral/provisional: constraint includes extraction (denial that restoration is achievable) — tangled_rope reading is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substitution_vs_deferral_semantics, conceptual, 'Whether Kodashim substitutes sacrifice or defers it').

omega_variable(
    memorial_vs_occupancy_ambiguity,
    'Does declaring Kodashim an ''archive'' or ''memorial'' functionally preserve it as an occupied kernel (live sacrificial law applicable when conditions allow) or declare it a foreclosed kernel (sacrificial law superseded and inapplicable)?',
    'Analysis of halakhic rulings: when restoration becomes theoretically possible (e.g., territorial control, temple site access, mass return to Israel), do authorities treat Kodashim as immediately actionable law or as historical record? Comparative analysis: how are other suspended-but-potentially-restorable commandments (e.g., certain agricultural laws) treated in halakhic literature?',
    'If ''archive'' = functionally foreclosed: constraint is pure substitution/rope. If ''archive'' = occupied kernel awaiting conditions: constraint is deferral/extraction — beneficiaries benefit from indefinite deferral.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(memorial_vs_occupancy_ambiguity, conceptual, 'Kodashim as foreclosed archive vs. occupied deferred kernel').

omega_variable(
    institutional_authority_extraction,
    'Does rabbinic text-study institutional authority structurally depend on maintaining the substitution against restoration movements?',
    'Historical analysis: did rabbinic authorities resist restoration movements not on halakhic grounds but to protect their interpretive authority? Does contemporary Jewish institutional leadership treat Kodashim as coordinate study (high-status intellectual activity) vs. as coordination mechanism (preservation against obsolescence)? Analysis of how institutions respond when communities attempt restoration-adjacent practices (e.g., studying Kodashim WITH intent to practice if conditions allow).',
    'If institutional authority extraction is real: suppression is higher than measured (0.58 → 0.70+), and the constraint is pure snare from restoration perspective. If institutional neutrality is genuine: extraction is coordination benefit, suppression is lower, and constraint is rope from institutional perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_authority_extraction, empirical, 'Whether rabbinic authority extraction maintains substitution against restoration').

omega_variable(
    restoration_movement_theoretic_feasibility,
    'Is the restoration of sacrificial practice theoretically feasible within rabbinic Judaism, or has rabbinic jurisprudence developed new binding principles that foreclose it?',
    'Legal-textual analysis: do contemporary rulings on ritual purity (tumah/taharah), priesthood qualification, or temple location establish absolute bars to restoration, or are these framed as contingent on current conditions? Investigation of whether rabbinic authorities have formally ruled restoration impossible vs. merely indefinitely deferred.',
    'If foreclosed: Kodashim is pure archive/monument, constraint is rope. If theoretically feasible but practically blocked: constraint is extraction/snare or tangled_rope (institutional blockage against structural possibility).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_movement_theoretic_feasibility, empirical, 'Theoretical feasibility of sacrificial restoration in rabbinic jurisprudence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substitution_archive, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_initial_early_rabbinic, substitution_archive, theater_ratio, 0, 0.42).
narrative_ontology:measurement(theater_midpoint_geonic_rishonim, substitution_archive, theater_ratio, 500, 0.58).
narrative_ontology:measurement(theater_contemporary_diaspora_norm, substitution_archive, theater_ratio, 1000, 0.68).

% Extraction over time
narrative_ontology:measurement(extr_initial_early_rabbinic, substitution_archive, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(extr_midpoint_geonic_rishonim, substitution_archive, base_extractiveness, 500, 0.48).
narrative_ontology:measurement(extr_contemporary_diaspora_norm, substitution_archive, base_extractiveness, 1000, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substitution_archive, enforcement_mechanism).
narrative_ontology:affects_constraint(substitution_archive, temple_restoration_institutional_barriers).
narrative_ontology:affects_constraint(substitution_archive, diaspora_authority_legitimacy_structure).

% DUAL FORMULATION NOTE:
% The substitution_archive reading is one component of a constraint family analyzing Kodashim's role in post-Temple Judaism. Sibling constraints (study_as_exercise, performance_only) decompose the kernel into structurally distinct claims with different extractiveness values. The substitution_archive reading (ε=0.52) emphasizes the extraction involved in claiming substitution is complete; the study_as_exercise reading would have lower extractiveness (ε ≈ 0.15) treating Kodashim as pure intellectual activity; the performance_only reading would have different victims (those seeking intellectual engagement rather than restoration). All three readings share the same base kernel (Kodashim) but instantiate different structural constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substitution_archive, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
