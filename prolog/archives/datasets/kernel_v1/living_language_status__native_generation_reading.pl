% ============================================================================
% CONSTRAINT STORY: living_language_status__native_generation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__native_generation_reading, []).

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
 *   constraint_id: living_language_status__native_generation_reading
 *   human_readable: Living Language Status (Native Generation Reading)
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   The native-generation reading of living language status defines a
 *   language as living only if native speakers transmit it
 *   intergenerationally as a mother tongue in daily life. This reading
 *   explicitly frames liturgical recitation, literary transmission, and
 *   scholarly study as preservation of dead or dying languages, regardless of
 *   their functional vitality in religious or intellectual contexts. This
 *   constraint is one reading of a contested kernel with two sibling
 *   readings: the liturgical_preservation_reading (a language is living if
 *   sacred texts are continuously recited and studied in ritual) and the
 *   literary_continuity_reading (a language is living if it remains a
 *   productive medium for new literary and intellectual work). The
 *   native-generation reading benefits secular nationalist movements seeking
 *   to establish linguistic sovereignty through majority native-speaker
 *   dominance in public life, while imposing extraction on liturgical-only
 *   communities whose language transmission does not fit the criterion. The
 *   constraint exhibits Tangled Rope classification: it has a genuine
 *   coordination function (incentivizing parent-child transmission as a
 *   preservation mechanism) alongside asymmetric extraction (rendering
 *   non-native transmission pathways illegitimate). The extractiveness value
 *   (0.52) reflects moderate intensity — the criterion requires institutional
 *   infrastructure for daily-life transmission but does not involve violent
 *   coercion. Suppression (0.65) is significant because communities not
 *   meeting the criterion face delegitimization, reduced resource access, and
 *   pressure to assimilate or abandon the language. Theater ratio (0.48)
 *   indicates substantive functional content: unlike purely performative
 *   constraints, the native-generation criterion shapes real resource
 *   allocation, education policy, and language revival priorities.
 *
 * KEY AGENTS:
 *   - Secular Nationalist Movement: Primary beneficiary (institutional/arbitrage) — gains legitimacy for linguistic sovereignty claims and institutional power to mandate native-language education and public life dominance
 *   - Liturgical-Only Communities: Primary victim (powerless/trapped) — their language transmission practice is classified as dead regardless of intergenerational continuity; exit requires abandoning religious identity
 *   - Minority Diaspora Language Users: Secondary victim (moderate/constrained) — benefit from native-generation legitimacy but face significant barriers to maintaining daily-life transmission without territorial/state support
 *   - Academic Sociolinguists: Organized beneficiary-victim (organized/mobile) — gain gatekeeping authority through the criterion but also constrain the research domain to languages meeting the standard
 *   - UNESCO Language Vitality Framework: Institutional actor (institutional/constrained) — adopted the criterion as formal assessment standard; operates as piton (performative rather than determinative)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a definitional choice as empirical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__native_generation_reading, 0.52).
domain_priors:suppression_score(living_language_status__native_generation_reading, 0.65).
domain_priors:theater_ratio(living_language_status__native_generation_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__native_generation_reading, "Living Language Status (Native Generation Reading)").
narrative_ontology:topic_domain(living_language_status__native_generation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(living_language_status__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__native_generation_reading, 'f2a2389d-4bea-45ed-8eaa-d0238408fe3c').
narrative_ontology:cs_kernel_codification('f2a2389d-4bea-45ed-8eaa-d0238408fe3c', distributed).
narrative_ontology:cs_authority_grounding('f2a2389d-4bea-45ed-8eaa-d0238408fe3c', distributed).
narrative_ontology:cs_reading_relation('f2a2389d-4bea-45ed-8eaa-d0238408fe3c', living_language_status__liturgical_preservation_reading, forecloses).
narrative_ontology:cs_reading_relation('f2a2389d-4bea-45ed-8eaa-d0238408fe3c', living_language_status__literary_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('f2a2389d-4bea-45ed-8eaa-d0238408fe3c', foundational, daily_life_transmission_necessary).
narrative_ontology:cs_axiom_status(daily_life_transmission_necessary, holdable).
narrative_ontology:cs_axiom_grounding('f2a2389d-4bea-45ed-8eaa-d0238408fe3c', daily_life_transmission_necessary, empirically_contingent).
narrative_ontology:cs_axiom('f2a2389d-4bea-45ed-8eaa-d0238408fe3c', secondary, liturgical_transmission_insufficient).
narrative_ontology:cs_axiom_status(liturgical_transmission_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('f2a2389d-4bea-45ed-8eaa-d0238408fe3c', liturgical_transmission_insufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('f2a2389d-4bea-45ed-8eaa-d0238408fe3c', secular_nationalist_linguistic_sovereignty).
narrative_ontology:cs_drift_state('f2a2389d-4bea-45ed-8eaa-d0238408fe3c', contemporary_global_development_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f2a2389d-4bea-45ed-8eaa-d0238408fe3c', '').
narrative_ontology:cs_kernel_id(living_language_status__native_generation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, secular_nationalist_movement).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, liturgical_only_communities).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, minority_diaspora_language_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LITURGICAL-ONLY COMMUNITY (SNARE) — Members of communities that transmit the language exclusively through religious ritual (Hebrew in diaspora synagogues, Latin in Catholic liturgy, Coptic in Coptic Orthodox communities) face maximal extraction under this reading. Their linguistic practice is classified as 'dead' regardless of intergenerational transmission through sacred contexts. Exit is structurally blocked: abandoning the liturgical practice means abandoning religious identity and community belonging. The reading imposes a frame that renders their genuine language transmission invisible and delegitimizes their claim to linguistic vitality.
constraint_indexing:constraint_classification(living_language_status__native_generation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MINORITY DIASPORA LANGUAGE USERS (TANGLED ROPE) — Language communities maintaining daily-life transmission in diaspora contexts (Yiddish-speaking Haredi communities, Arabic-speaking immigrant families, Icelandic in diaspora) benefit from the native-generation legitimacy criterion — their practice is recognized as 'living.' But they also face significant extraction: the reading requires constant justification of daily-life transmission against assimilationist pressures, resource constraints for maintaining immersion contexts, and institutional barriers to creating spaces where the language is genuinely dominant. Coordination function: the criterion incentivizes parent-child transmission as the primary preservation mechanism. Extraction: agents must continuously demonstrate native-speaker status and daily-life use, creating surveillance of authenticity and identity verification.
constraint_indexing:constraint_classification(living_language_status__native_generation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SECULAR NATIONALIST MOVEMENT (ROPE) — This perspective benefits from the native-generation reading as a legitimacy criterion. The reading supports nationalist linguistic sovereignty claims: a nation's language is 'living' only when native speakers dominate public institutions and daily life, creating a structural incentive to prioritize majority-language dominance in education, media, and government. The reading provides institutional leverage for language revival projects that require mandatory education in the target language (Modern Hebrew, Revived Cornish, Icelandic in Iceland). The movement experiences the reading as pure coordination — it solves the problem of definitionally securing national linguistic homogeneity.
constraint_indexing:constraint_classification(living_language_status__native_generation_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ACADEMIC SOCIOLINGUISTS (TANGLED ROPE) — Organized professional community that uses the native-generation criterion in language vitality assessment. Benefits: the criterion provides a clear operationalizable metric for research (native speaker fluency levels, intergenerational transmission rates, percentage of population using language daily). Extraction: the criterion's application requires gatekeeping authority — sociolinguists become arbiters of which languages 'count' as living, granting them institutional power over language policy and funding allocation. The criterion also narrows the research domain: languages without native daily-use transmission are classified as non-viable, reducing the universe of languages worth studying and allocating resources to support.
constraint_indexing:constraint_classification(living_language_status__native_generation_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: UNESCO LANGUAGE VITALITY FRAMEWORK (PITON) — International institutional framework that adopted the native-generation criterion as one axis of language vitality assessment (UNESCO Red Book of Endangered Languages). The framework originally served a genuine coordination function — enabling comparative assessment across thousands of languages. But the criterion has become substantially performative: UNESCO's classifications are cited in policy debates but rarely determine resource allocation; the list is updated infrequently and reflects political negotiation rather than linguistic reality. The framework persists through institutional inertia — it is the established standard for international language policy discourse — despite known limitations in its classification logic.
constraint_indexing:constraint_classification(living_language_status__native_generation_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational and universal perspective, the native-generation criterion reflects a 'natural' or invariant fact about how languages persist: any language without intergenerational native-speaker transmission inevitably dies within 1-2 centuries, regardless of liturgical or literary use. This perspective reads the native-generation criterion as a discovery of an immutable demographic law. However, the structural data reveals this as a false summit: the criterion naturalizes a choice about which transmission pathways 'count' as preserving language vitality — a definitional question, not an empirical law. The reading's beneficiary structure (secular nationalist movement) and victim structure (liturgical-only communities) show that the 'natural law' framing masks a contested normative claim.
constraint_indexing:constraint_classification(living_language_status__native_generation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__native_generation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(living_language_status__native_generation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(living_language_status__native_generation_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(living_language_status__native_generation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(living_language_status__native_generation_reading, TR),
    TR >= 0.70.

:- end_tests(living_language_status__native_generation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The native-generation criterion creates asymmetric costs and benefits: secular nationalist movements gain institutional leverage to mandate native-language education and public space dominance, while liturgical-only communities lose legitimacy and resource access regardless of their language transmission success. The extraction is not maximal (0.72+) because the criterion lacks violent enforcement mechanisms and because some communities (diaspora daily-life transmitters) benefit from it. The measurement trajectory (0.35 → 0.52) reflects increasing institutional weaponization of the criterion in language policy over the 20-year interval — as nationalist language revival projects mature and UNESCO standards become more influential in development funding decisions, the asymmetric benefits to nationalist movements and costs to liturgical-only communities have intensified. Suppression (0.65): High. Significant barriers constrain alternatives to native-generation transmission: (1) state language policy privileges education in native-dominant languages; (2) digital media and cultural prestige favor daily-life languages over liturgical ones; (3) intergenerational transmission of non-native-dominant languages faces social pressure toward assimilation; (4) resource allocation for language preservation is increasingly tied to vitality metrics that privilege native-speaker transmission. The measurement trajectory (0.45 → 0.65) shows increasing suppression as language policy becomes more explicitly tied to native-generation criteria. Theater ratio (0.48): Moderate-to-low. The criterion has genuine functional content — it does incentivize parent-child transmission and shapes resource allocation in measurable ways — but it also functions partly as a ritual performance of scientific linguistics authority. The relatively stable trajectory (0.52 → 0.48) suggests the criterion maintains steady functional and performative balance rather than drifting toward pure theater.
 *
 * PERSPECTIVAL GAP:
 *   This reading produces maximal perspectival divergence. Liturgical-only communities (powerless/trapped) classify the constraint as Snare — their language transmission is rendered invisible and illegitimate. Diaspora daily-life transmitters (moderate/constrained) classify it as Tangled Rope — they gain legitimacy but face high costs. The nationalist movement (institutional/arbitrage) classifies it as Rope — pure coordination aligned with their goals. Sociolinguists (organized/mobile) classify it as Tangled Rope — they gain gatekeeping authority but constrain research scope. UNESCO (institutional/constrained) classifies it as Piton — the standard is performative rather than determinative. The analytical observer (analytical/analytical) risks classifying it as Mountain — a natural law of language death — revealing a false summit. The perspectival gaps directly instantiate the kernel contest: the readings coexist as live positions held by different constituencies (secular nationalist institutions, religious minorities, literary intellectuals), and no single party can adopt all three readings simultaneously without internal contradiction.
 *
 * DIRECTIONALITY LOGIC:
 *   The native-generation reading's directionality structure is defined by its beneficiary and victim relationship. Secular nationalist movements derive d ≈ 0.05 (full beneficiary position with arbitrage exit options) → f(d) ≈ -0.12 (negative/low effective extraction). Liturgical-only communities derive d ≈ 0.95 (full victim position with trapped exit options) → f(d) ≈ 1.42 (maximum experienced extraction). Diaspora daily-life transmitters derive d ≈ 0.55 (mixed position: legitimated by criterion but constrained by barriers) → f(d) ≈ 0.75 (moderate-high extraction). The scope modifier σ(S) reflects that the constraint operates at global scope (σ ≈ 1.2) — UNESCO standards and international language policy create verification complexity and cross-border normative enforcement. The formula χ = ε × f(d) × σ(S) produces effective extraction ranging from near-zero for beneficiaries to 0.88 for trapped victims at the base ε of 0.52.
 *
 * MANDATROPHY ANALYSIS:
 *   The native-generation reading avoids mandatrophy by acknowledging its own contested status. The reading is not a universal descriptive claim about language death (which would require mountain classification) but a normative definitional framework that reflects specific institutional interests (secular nationalist movements). The constraint's true classification as Tangled Rope (not mountain or rope) correctly represents that the criterion has genuine coordination function (incentivizing parent-child transmission) alongside asymmetric extraction (delegitimizing alternative transmission pathways). The perspectival divergence across all six perspectives shows that the constraint's meaning and force fundamentally depend on the observer's structural position relative to it. This dependency on perspective is the correct diagnosis — it prevents falsely naturalizing a definitional choice as empirical law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transmission_pathway_classification,
    'What makes one transmission pathway (daily-life parent-child speech) legitimately preserve language vitality while another pathway (liturgical recitation, literary transmission) does not?',
    'Empirical analysis of language death trajectories: do languages maintained through liturgical-only transmission survive longer, shorter, or equivalently to those lost by diaspora assimilation? Do literary-only languages (Medieval Latin, Classical Chinese in non-vernacular contexts) show measurable degradation in structural complexity or innovation?',
    'If liturgical/literary transmission shows empirically equivalent survival curves: the native-generation criterion is a normative choice, not an empirical discovery — the constraint reclassifies from mountain to tangled_rope across all perspectives, and the beneficiary structure becomes the primary analytical focus. If native-generation transmission shows superior survival: the criterion''s mountain status is partially warranted, though the question remains whether the difference is empirical or definitional.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transmission_pathway_classification, empirical, 'Whether different transmission pathways have equivalent or divergent language survival curves').

omega_variable(
    reading_kernel_contest,
    'Which reading of the living_language_status kernel represents the actual structural constraint: native_generation_reading, liturgical_preservation_reading, or literary_continuity_reading?',
    'This is irreducible to empirical data. The three readings are incommensurable definitional frameworks — each makes a different claim about what legitimately constitutes language vitality. The resolution depends on which authority structure is recognized as legitimate: secular nationalist movements (native generation), religious institutional hierarchies (liturgical preservation), or literary/intellectual communities (literary continuity). The engine routes this to cross-reading coupling analysis and authority-grounding assessment in the commitment-system structure.',
    'If native_generation_reading is accepted as authoritative: liturgical and literary transmission communities lose institutional legitimacy and resource access. If liturgical_preservation_reading is accepted: the native-generation criterion is revealed as a secular imposition on communities with different language transmission norms. If literary_continuity_reading is accepted: the native-generation criterion is overspecified — language vitality does not require daily-life native transmission, only productive intellectual/literary work.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_contest, conceptual, 'The kernel contest: which reading legitimately defines language vitality').

omega_variable(
    diaspora_transmission_sustainability,
    'Can diaspora communities maintain native-generation daily-life transmission across multiple generations without territorial concentration or state institutional support?',
    'Longitudinal sociolinguistic study of diaspora minority languages: intergenerational transmission rates in communities with and without institutional language education, media support, and endogamy norms. Historical case studies (Yiddish in North America, Italian in Argentina, Japanese in Hawaii) tracking whether daily-life transmission persists across 3+ generations without homeland state support.',
    'If transmission is unsustainable without territorial/institutional support: the native-generation criterion becomes structurally coupled to nationalist projects — the reading''s beneficiary (secular nationalist movement) becomes its sine qua non. If transmission persists in diaspora: the criterion is theoretically neutral but practically biased toward nations with state power to create native-speaker environments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diaspora_transmission_sustainability, empirical, 'Whether diaspora communities can maintain native-speaker transmission without state institutional support').

omega_variable(
    liturgical_community_identity_fusion,
    'For members of liturgical-only communities (especially religious minorities), is language transmission through sacred texts identity-locked or constrained?',
    'Ethnographic and interview-based study of language choice and identity in communities where the sacred language (Hebrew in diaspora Judaism, Coptic in Coptic Orthodox, Latin in traditional Catholicism, Classical Arabic in Quranic Islam) is transmitted through liturgy but not daily life. Analysis of whether members perceive the choice to use the language daily as structurally possible but costly (constrained) or as identity-threatening (identity_locked).',
    'If identity_locked: the native-generation reading imposes a frame that would require these communities to abandon religious identity to satisfy the criterion — the extraction is cognitive/identity-based rather than merely structural. If constrained: the reading creates high costs but theoretically surmountable barriers to compliance. The distinction informs whether the reading''s suppression value should be revised upward (identity_locked → effective suppression approaches 1.0) or is accurately characterized at 0.65 (constrained → significant barriers but exit theoretically available).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_community_identity_fusion, empirical, 'Whether liturgical-only communities experience the native-generation criterion as identity-locked or constrained').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__native_generation_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lls_native_tr_t0, living_language_status__native_generation_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(lls_native_tr_t10, living_language_status__native_generation_reading, theater_ratio, 10, 0.5).
narrative_ontology:measurement(lls_native_tr_t20, living_language_status__native_generation_reading, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(lls_native_be_t0, living_language_status__native_generation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lls_native_be_t10, living_language_status__native_generation_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(lls_native_be_t20, living_language_status__native_generation_reading, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(lls_native_su_t0, living_language_status__native_generation_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(lls_native_su_t10, living_language_status__native_generation_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(lls_native_su_t20, living_language_status__native_generation_reading, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__native_generation_reading, identity_coordination).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, living_language_status__liturgical_preservation_reading).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, living_language_status__literary_continuity_reading).

% DUAL FORMULATION NOTE:
% The living_language_status kernel has three structurally distinct constraint readings with different ε values and beneficiary/victim structures. Each reading must be authored as a separate story because their ε values differ: native_generation_reading (ε=0.52, Tangled Rope) emphasizes institutional infrastructure costs for daily-life transmission; liturgical_preservation_reading (ε≈0.30-0.35, Rope or Tangled Rope) emphasizes continuity of textual/ritual transmission with lower extraction; literary_continuity_reading (ε≈0.25-0.30, Rope) emphasizes intellectual productivity with minimal extraction. All three readings share the same kernel (contested definition of language vitality) but instantiate different constraints. The readings coexist as live positions held by different constituencies; the constraint family traces how the same domain question produces incommensurable definitions depending on authority grounding.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(living_language_status__native_generation_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
