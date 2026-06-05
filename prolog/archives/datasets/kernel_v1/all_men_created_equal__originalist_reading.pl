% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__originalist_reading, []).

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
 *   constraint_id: all_men_created_equal__originalist_reading
 *   human_readable: Equality Bounded by 18th-Century Social Taxonomy (Originalist Reading)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The originalist reading of 'all men are created equal' instantiates one
 *   specific constraint within a contested kernel. This reading interprets
 *   the Declaration and Constitution to mean that equality protections apply
 *   only to those whom the 18th-century founders recognized as 'men' and
 *   'people' — a category that explicitly excluded enslaved Africans (counted
 *   as three-fifths persons), indigenous peoples, women, and non-propertied
 *   males. The originalist reading anchors constitutional meaning to the
 *   founders' intent and the social taxonomy of their era. This creates a
 *   structural tension: the Declaration employs universal language ('all
 *   men,' 'created equal') while the originalist reading systematically
 *   narrows these universals to a restricted set. The constraint benefits the
 *   founding elite and their descendants by preserving the property and power
 *   arrangements the founders established. It extracts from enslaved peoples,
 *   indigenous nations, women, and all who were excluded from the original
 *   taxon. Unlike the textualist paradox reading (which treats the tension
 *   between universal language and restricted application as a logical
 *   contradiction that delegitimizes originalism), the originalist reading
 *   resolves the tension by treating the social taxon itself as the semantic
 *   boundary of the universal language. Unlike the universalist reading
 *   (which treats the language as semantically universal and therefore
 *   requiring iterative expansion), the originalist reading treats the
 *   founders' intentions as the semantic constraint. The constraint's theater
 *   ratio increases over time (0.42 → 0.58) as the tension between the
 *   language's apparent universalism and its restricted application becomes
 *   more salient and requires more rhetorical work to maintain.
 *
 * KEY AGENTS:
 *   - Founding Elite and Male Propertied Descendants: Primary beneficiary (institutional/arbitrage) — capture the benefits of equality language without bearing its costs; originalist reading anchors their privileges in 'the Constitution's true meaning'
 *   - Enslaved Africans and Descendants: Primary victim (powerless/trapped) — systematically excluded by the social taxon; bear maximum extraction without exit or voice
 *   - Indigenous Peoples: Primary victim (powerless/trapped) — excluded entirely from the framework; bear territorial and sovereignty extraction enforced through the constraint
 *   - Women: Secondary victim (powerless/trapped) — excluded from the social taxon of 'men'; bear extraction through denial of political and civil rights
 *   - Constitutional Expansion Advocates: Secondary victim (organized/constrained) — organized movements seeking to expand the scope of equality; bear extraction through interpretive narrowing; constrained by the constraint's institutional power
 *   - Originalist Judicial Authority: Institutional actor (institutional/arbitrage) — wields interpretive power to maintain the constraint; benefits from the authority originalism grants them; experiences the constraint as legitimate constitutional method
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__originalist_reading, 0.62).
domain_priors:suppression_score(all_men_created_equal__originalist_reading, 0.68).
domain_priors:theater_ratio(all_men_created_equal__originalist_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__originalist_reading, snare).
narrative_ontology:human_readable(all_men_created_equal__originalist_reading, "Equality Bounded by 18th-Century Social Taxonomy (Originalist Reading)").
narrative_ontology:topic_domain(all_men_created_equal__originalist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(all_men_created_equal__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__originalist_reading, '7435310a-2c09-4142-ae89-2399a4cc803e').
narrative_ontology:cs_kernel_codification('7435310a-2c09-4142-ae89-2399a4cc803e', fixed_text).
narrative_ontology:cs_authority_grounding('7435310a-2c09-4142-ae89-2399a4cc803e', extraction).
narrative_ontology:cs_interpretation_layer_present('7435310a-2c09-4142-ae89-2399a4cc803e').
narrative_ontology:cs_reading_relation('7435310a-2c09-4142-ae89-2399a4cc803e', all_men_created_equal__textualist_paradox_reading, forecloses).
narrative_ontology:cs_reading_relation('7435310a-2c09-4142-ae89-2399a4cc803e', all_men_created_equal__universalist_reading, coexists_with).
narrative_ontology:cs_axiom('7435310a-2c09-4142-ae89-2399a4cc803e', foundational, meaning_fixed_at_founding_intent).
narrative_ontology:cs_axiom_status(meaning_fixed_at_founding_intent, holdable).
narrative_ontology:cs_axiom_grounding('7435310a-2c09-4142-ae89-2399a4cc803e', meaning_fixed_at_founding_intent, conventional).
narrative_ontology:cs_axiom('7435310a-2c09-4142-ae89-2399a4cc803e', secondary, id_18th_century_social_taxon_governs_semantic_scope).
narrative_ontology:cs_axiom_status(id_18th_century_social_taxon_governs_semantic_scope, overridden).
narrative_ontology:cs_axiom_grounding('7435310a-2c09-4142-ae89-2399a4cc803e', id_18th_century_social_taxon_governs_semantic_scope, empirically_contingent).
narrative_ontology:cs_reference_frame('7435310a-2c09-4142-ae89-2399a4cc803e', id_18th_century_social_taxon_bounded_equality).
narrative_ontology:cs_drift_state('7435310a-2c09-4142-ae89-2399a4cc803e', contemporary_civil_rights_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7435310a-2c09-4142-ae89-2399a4cc803e', '2026-02-27T00:00:00Z').
narrative_ontology:cs_kernel_id(all_men_created_equal__originalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, founding_elite_and_male_propertied_descendants).
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, constitutional_authority_grounded_in_original_intent).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, enslaved_africans_and_descendants).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, indigenous_peoples).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, women).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, non_propertied_males).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, constitutional_expansion_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENSLAVED AFRICANS AND DESCENDANTS (SNARE) — Trapped within a constitutional framework that explicitly denies their humanity ('three-fifths persons'). The Declaration's universal language is theatrically invoked but systematically negated by originalist interpretation that narrows equality to the 18th-century taxon. No exit from the framework; no capacity to contest the reading. Maximum experienced extraction. The constraint's suppression mechanism is enforced through law and violence.
constraint_indexing:constraint_classification(all_men_created_equal__originalist_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INDIGENOUS PEOPLES (SNARE) — Excluded entirely from the 'we the people' framework. The originalist reading confines equality protections to those recognized as people by 18th-century taxonomy — which systematically excluded indigenous nations. Trapped, powerless, and bearing maximum extraction. The constraint operates as a legal mechanism for territorial and sovereignty dispossession.
constraint_indexing:constraint_classification(all_men_created_equal__originalist_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: FOUNDING ELITE AND CONSTITUTIONAL AUTHORITY (ROPE) — Benefits from the originalist reading that anchors constitutional authority to their intent. The constraint solves their coordination problem: it stabilizes the legitimacy of their property, power, and exclusionary social order under the language of universal principles. Experience the constraint as benign coordination — the Declaration's universalism is successfully bounded by originalist interpretation to preserve their privileges. Low or negative experienced extraction.
constraint_indexing:constraint_classification(all_men_created_equal__originalist_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL EXPANSION ADVOCATES (TANGLED ROPE) — Organized movements (abolitionists, suffragists, civil rights) that seek to expand equality protections beyond the 18th-century taxon. They benefit from the universal language of the Declaration (coordination foundation) but are systematically extracted against by originalist interpretation that narrows scope. High constraint on expansion; high asymmetric extraction. Cannot exit without abandoning the constitutional framework itself. The constraint coordinates around a universal principle while extracting through originalist interpretation.
constraint_indexing:constraint_classification(all_men_created_equal__originalist_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW FRAMING (MOUNTAIN) — From a civilizational perspective, the originalist reading presents itself as a fixed legal principle: the Constitution means what the founders intended it to mean, and this is an immutable rule of constitutional interpretation. The reading frames founder intent as a natural law of legal hermeneutics — unchangeable, objective, and discoverable. However, the structural data reveals this as a false summit: originalism is a contingent interpretive method, not a law of nature. The apparent immutability derives from institutional power (control of judicial interpretation and legal pedagogy), not from logical necessity.
constraint_indexing:constraint_classification(all_men_created_equal__originalist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__originalist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(all_men_created_equal__originalist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(all_men_created_equal__originalist_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(all_men_created_equal__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High-moderate. The originalist reading extracts substantially from those excluded by the 18th-century taxon — it denies them constitutional protection while invoking universal language. The extractiveness is not at maximum (0.75) because constitutional doctrine has been forced to accommodate subsequent expansions (13th, 14th, 15th, 19th amendments), reducing the constraint's raw extractive power. However, originalist interpretation has worked to minimize the scope of these amendments, keeping extractiveness high. Suppression (0.68): High. The constraint is maintained through legal enforcement (courts, police, military), institutional inertia, rhetorical work to justify the taxon boundaries, and the powerlessness of excluded groups to contest the interpretation. Suppression includes both active enforcement (laws excluding women from property, enslaving Africans) and structural barriers (lack of standing to challenge, lack of voice in interpretation). Theater ratio (0.58): Moderate-high and increasing. The originalist reading requires continuous rhetorical work to justify why universal language applies only to the 18th-century taxon. The theater is performative because the claim that the founders 'truly meant' equality to be taxon-bounded requires constant reinterpretation and selection among conflicting founder statements. The theater increases over time as the tension becomes more salient — contemporary originalism must work harder to justify the taxon boundaries than founding-era originalism did.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces dramatic perspectival divergence. Enslaved peoples and indigenous nations see a snare: universal language that excludes them, enforced through violence, with no exit or contest. Constitutional expansion advocates see tangled rope: the universal language provides coordination foundation and grounds for expansion appeals, but originalist interpretation systematically forecloses expansion. The founding elite see rope: the constraint successfully coordinates their commonwealth while excluding those who would disrupt it. The analytical observer sees a false summit: the originalist reading presents itself as discovering fixed constitutional meaning ('the Constitution means what the founders intended'), but this is a contingent interpretive choice, not a natural law. The mask of interpretation is the mechanism of extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality flows from the agent's structural position. Enslaved peoples and indigenous nations are trapped victims — d approaches 1.0, producing maximum experienced extractiveness. Constitutional expansion advocates are organized but constrained — they have some power to mobilize (d ≈ 0.65) but face institutional barriers to reinterpreting the constraint. The founding elite are beneficiaries with arbitrage options — they can switch between originalism and other constitutional readings as convenient (d ≈ 0.15), experiencing low or negative effective extraction. Originalist judicial authority experiences the constraint as legitimate method grounding their interpretive power (d ≈ 0.10), producing near-zero or negative experienced extractiveness because the constraint enhances their authority. The perspectival gap reveals that the 'objective' constitutional meaning the originalist reading claims is actually a distribution of interpretive power — beneficiaries experience it as objective law; victims experience it as extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by recognizing that this constraint is one reading of a contested kernel, not an objective constitutional fact. The originalist reading faces a fatal structural problem: it must explain why universal language ('all men created equal') applies only to the 18th-century taxon. It does this through the claim that 'original intent' bounds the universalism. This claim is coherent within originalism's framework. However, the originalist reading is simultaneously a snare (from victim perspectives) and a rope (from beneficiary perspectives) — the same structural mechanism produces opposite classifications. This is not a contradiction; it is the diagnostic signature of a contingent institutional constraint being naturalized as fixed law. The mandatrophy is resolved by acknowledging that 'the Constitution's true meaning' is not an object to be discovered but a site of contestation. Each reading (originalist, textualist paradox, universalist) has internal coherence but conflicting authority claims. The question is not which reading is correct but which power structures have the authority to enforce their reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_discoverability,
    'Is the founders'' original intent actually discoverable from historical text and context, or is the invocation of ''original intent'' itself a constructive act that projects contemporary values backward?',
    'Historiographical analysis of primary sources; comparison of originalist judicial opinions over time to assess consistency in intent recovery; epistemic analysis of projection mechanisms in historical interpretation',
    'If intent is genuinely discoverable: originalist reading has epistemic legitimacy as constraint-discovery rather than constraint-construction. If intent is constructed retrospectively: originalism is an interpretive method that naturalizes its own choices; the constraint''s claimed immutability is false.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_intent_discoverability, conceptual, 'Whether founders'' original intent is discoverable or constructed retrospectively').

omega_variable(
    founders_intent_whose_intent,
    'Whose intent among the founders governs when founders disagreed radically on slavery, representation, and the reach of equality?',
    'Analysis of founding documents and debates; identification of consensus positions vs contested positions among founders; examination of which founders'' views originalist jurisprudence selects and which it marginalizes',
    'If consensus intent chosen: originalism claims plausible legitimacy. If contested intent selected arbitrarily: originalism masks a choice to privilege some founders'' vision over others, making it a tool of extraction rather than principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founders_intent_whose_intent, conceptual, 'Which founders'' intent governs when founders disagreed').

omega_variable(
    equality_language_universal_or_taxon_bound,
    'Does the Declaration''s language ''all men are created equal'' carry universal semantic meaning, or is its meaning strictly bounded to the 18th-century social taxon to which it was applied?',
    'Linguistic and semantic analysis of the phrase; comparison to how contemporary interpreters use universal quantifiers; historical analysis of how the phrase was immediately understood by expansionists vs restrictivists in the founding era',
    'If universal: the original intent includes the principle of universality, and originalism contradicts itself by bounding equality to the 18th-century taxon. If taxon-bound: the language means what it was applied to mean, supporting originalist narrowing. This determines whether originalism instantiates a performative contradiction or a coherent interpretive position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equality_language_universal_or_taxon_bound, conceptual, 'Whether ''all men created equal'' carries universal semantic meaning').

omega_variable(
    interpretive_authority_grounding,
    'What legitimate source grounds the authority to interpret the Constitution as meaning what the founders intended? Whose authority and by what warrant?',
    'Jurisprudential analysis of the sources cited for originalist authority (Framers'' writings, historical consensus, legal tradition, textual fidelity); comparison to alternative grounding sources (living Constitution authority, democratic legitimacy of subsequent interpreters); examination of how authority is actually wielded vs how it is theoretically justified',
    'If originalist authority is genuinely grounded in discoverable intent: the constraint derives legitimacy from epistemic access to the past. If authority is constructed through institutional power (judicial control, legal pedagogy dominance): the constraint is extractive despite its universalist language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_grounding, conceptual, 'What legitimate source grounds interpretive authority for originalism').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Does originalist reading logically foreclose the universalist reading of the Declaration, or can both readings coexist within a framework that acknowledges their irreducible tension?',
    'Logical analysis of whether originalism''s core premise (meaning is fixed at founding intent) necessarily rules out universalism''s core premise (meaning evolves through democratic interpretation). Institutional analysis of whether courts can simultaneously apply both readings to different aspects of constitutional law.',
    'If foreclosure: this reading rules out the universalist reading; the kernel cannot hold both commitments simultaneously. If coexistence: both readings are live options held by different parties in an ongoing constitutional dispute, making the constraint''s classification context-dependent rather than absolute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether originalism forecloses universalism or permits coexistence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__originalist_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(amce_orig_theater_t0_founding, all_men_created_equal__originalist_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(amce_orig_theater_t100_post_abolition, all_men_created_equal__originalist_reading, theater_ratio, 100, 0.5).
narrative_ontology:measurement(amce_orig_theater_t200_contemporary, all_men_created_equal__originalist_reading, theater_ratio, 200, 0.58).

% Extraction over time
narrative_ontology:measurement(amce_orig_extract_t0_founding, all_men_created_equal__originalist_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(amce_orig_extract_t100_post_abolition, all_men_created_equal__originalist_reading, base_extractiveness, 100, 0.68).
narrative_ontology:measurement(amce_orig_extract_t200_contemporary, all_men_created_equal__originalist_reading, base_extractiveness, 200, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(amce_orig_suppress_t0_founding, all_men_created_equal__originalist_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(amce_orig_suppress_t100_post_abolition, all_men_created_equal__originalist_reading, suppression_requirement, 100, 0.72).
narrative_ontology:measurement(amce_orig_suppress_t200_contemporary, all_men_created_equal__originalist_reading, suppression_requirement, 200, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, all_men_created_equal__textualist_paradox_reading).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, all_men_created_equal__universalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, slavery_constitutional_accommodation).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, women_excluded_from_political_equality).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, indigenous_peoples_excluded_from_citizenship).

% DUAL FORMULATION NOTE:
% The 'all men created equal' kernel contains three structurally distinct constraint readings with different extractiveness values. The originalist reading (this file, ε=0.62) represents one resolution of the universal language / restricted application tension. The textualist paradox reading (ε=0.58) treats the tension as logical contradiction. The universalist reading (ε=0.35) treats the language as universally binding. These are not three measurements of one constraint but three different constraints that share the same kernel language. Each has its own beneficiary/victim structure, its own classification profile, and its own authority claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(all_men_created_equal__originalist_reading, institutional, 0.12).
constraint_indexing:directionality_override(all_men_created_equal__originalist_reading, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
