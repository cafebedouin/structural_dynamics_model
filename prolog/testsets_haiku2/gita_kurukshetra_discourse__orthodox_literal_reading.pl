% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__orthodox_literal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_orthodox_literal, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: gita_kurukshetra_discourse__orthodox_literal_reading
 *   human_readable: Gita Kurukshetra Discourse - Orthodox Literal Reading
 *   domain: religious/philosophical/ethical
 *
 * SUMMARY:
 *   The Bhagavad Gita's Kurukshetra discourse is a kernel text subject to
 *   multiple readings. This constraint instantiates the ORTHODOX LITERAL
 *   reading: the text mandates caste-based duty (svadharma) as cosmically
 *   ordained and legitimates righteous violence (dharmic yuddha) when a
 *   warrior fulfills that duty. Under this reading, Arjuna's hesitation to
 *   fight is portrayed as a spiritual/ethical error; Krishna's exhortation to
 *   battle is a divine command to fulfill warrior caste duty without
 *   attachment or doubt. The Brahmin priestly class maintains interpretive
 *   authority to define what dharma requires in each case. Lower castes are
 *   locked in hereditary duty by the same text. This reading has operated as
 *   institutional constraint in Hindu-majority polities and continues to do
 *   so. The constraint is CLAIMED as tangled_rope (genuine coordination
 *   function of social order) while the authored metrics describe heavily
 *   extractive, actively suppressed operation — the claim and metrics are
 *   independent authored facts; the divergence is the measurement the corpus
 *   takes.
 *
 * KEY AGENTS:
 *   - Brahmin priestly class: institutional agenda-setter maintaining interpretive monopoly; derives authority and material support from the reading
 *   - Kshatriya warrior class: powerful beneficiary legitimated in violence by dharmic duty framing
 *   - Lower castes (Shudra, untouchables): powerless victims locked by birth in service roles; identity_locked exit
 *   - Women across castes: powerless victims bound by gender-caste double constraint; identity_locked
 *   - Hindu reform movements: organized observers attempting to reframe the kernel without abandoning it
 *   - Dissident philosophical voices: excluded from orthodox authority structure; would contest the reading if heard
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, 0.82).
domain_priors:suppression_score(gita_kurukshetra_discourse__orthodox_literal_reading, 0.87).
domain_priors:theater_ratio(gita_kurukshetra_discourse__orthodox_literal_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__orthodox_literal_reading, tangled_rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__orthodox_literal_reading, "Gita Kurukshetra Discourse - Orthodox Literal Reading").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__orthodox_literal_reading, "religious/philosophical/ethical").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__orthodox_literal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__orthodox_literal_reading, 'abccd4ef-41cb-4729-95b2-6fb443e4d526').
narrative_ontology:cs_kernel_codification('abccd4ef-41cb-4729-95b2-6fb443e4d526', fixed_text).
narrative_ontology:cs_authority_grounding('abccd4ef-41cb-4729-95b2-6fb443e4d526', lineage).
narrative_ontology:cs_interpretation_layer_present('abccd4ef-41cb-4729-95b2-6fb443e4d526').
narrative_ontology:cs_reading_relation('abccd4ef-41cb-4729-95b2-6fb443e4d526', gita_kurukshetra_discourse__gandhian_allegorical_reading, coexists_with).
narrative_ontology:cs_reading_relation('abccd4ef-41cb-4729-95b2-6fb443e4d526', gita_kurukshetra_discourse__universalist_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('abccd4ef-41cb-4729-95b2-6fb443e4d526', foundational, caste_duty_cosmically_ordained).
narrative_ontology:cs_axiom_status(caste_duty_cosmically_ordained, holdable).
narrative_ontology:cs_axiom_grounding('abccd4ef-41cb-4729-95b2-6fb443e4d526', caste_duty_cosmically_ordained, deontological).
narrative_ontology:cs_axiom('abccd4ef-41cb-4729-95b2-6fb443e4d526', foundational, righteous_violence_in_kshatriya_dharma).
narrative_ontology:cs_axiom_status(righteous_violence_in_kshatriya_dharma, holdable).
narrative_ontology:cs_axiom_grounding('abccd4ef-41cb-4729-95b2-6fb443e4d526', righteous_violence_in_kshatriya_dharma, deontological).
narrative_ontology:cs_reference_frame('abccd4ef-41cb-4729-95b2-6fb443e4d526', vedic_caste_order_eternal).
narrative_ontology:cs_drift_state('abccd4ef-41cb-4729-95b2-6fb443e4d526', modern_reform_challenge_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('abccd4ef-41cb-4729-95b2-6fb443e4d526', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_interpretive_authority).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_class).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, varna_hierarchy_order).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, lower_castes_shudra_untouchables).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, war_casualties_non_combatants).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, women_bound_by_caste_duty).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, dissident_philosophical_voices).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_priestly_class).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains exclusive authority to interpret scriptural texts and arbitrate dharmic obligations. Controls the reading that legitimates the varna order and extracts ritual authority and material support across castes. Their position depends on the text's literal reading mandating Brahmin interpretive monopoly and ritual supremacy. They have arbitrage-grade exit through reinterpretation or migration to other institutional roles, yet remain bound by the institutional identity that supplies their authority.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_priestly_class, agenda_setter,
    institutional, civilizational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_priestly_class, beneficiary).

% Derives legitimacy for martial violence from the text's dharmic duty framing. The orthodox reading consecrates their warrior function and their violence in service of caste order as righteous, even mandated. They gain moral and social permission to deploy violence that lower castes cannot claim. Yet their exit is constrained by warrior identity and the duty framework itself—abandoning the dharmic framing would dissolve their legitimacy.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_class, beneficiary,
    powerful, generational, constrained, regional).

% The four-varna system (Brahmin, Kshatriya, Vaishya, Shudra) and the underlying principle of birth-determined duty. The orthodox reading vindicates this order as divinely ordained and enforces its persistence by making deviation from caste duty itself a violation of cosmic law. As a non-agent, this entry documents a vindicated proposition: the varna system itself is what the constraint legitimates.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, varna_hierarchy_order, beneficiary,
    institutional, civilizational, analytical, regional).
narrative_ontology:stakeholder_non_agent(gita_kurukshetra_discourse__orthodox_literal_reading, varna_hierarchy_order).

% Locked in service roles and ritual impurity by birth under the text's caste mandates. The orthodox reading legitimates their subordination as divinely willed and their duty as service to higher castes without reciprocal obligation. They bear the costs of hierarchy—material extraction through taxes and forced labor, ritual exclusion from temples and sacred knowledge, legal disability in property and testimony rights—with no structural path to exit or equality. Their identity (born into the varna) is the mechanism of lock-in; conversion, migration, or profession-change are culturally coded as violations of dharma.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, lower_castes_shudra_untouchables, payer,
    powerless, civilizational, identity_locked, regional).

% Killed or displaced in the dharmic war that the text's reading legitimates. Non-combatants, civilians, and the already-weak die in a conflict where one side's violence is consecrated as righteous duty. The constraint operates their deaths as acceptable cost of the victor's dharmic obligation. They have zero exit options; their death is the operative mechanism through which the constraint's violence is implemented.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, war_casualties_non_combatants, payer,
    powerless, immediate, trapped, local).

% Constrained by both gender and caste norms under the text's reading. Their duties are defined by their birth (varna) and their relational roles (wife, daughter, widow). Widows of fallen warriors and lower-caste women bear reproductive and service burdens mandated by the same orthodoxy that legitimates the war. A Brahmin woman has higher status than a lower-caste man, but both are locked in gender-role expectations by the same reading. Their identity-lock is compounded by the intersection of gender and caste.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, women_bound_by_caste_duty, payer,
    powerless, civilizational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__orthodox_literal_reading, women_bound_by_caste_duty, payer).

% Philosophies and readings that reject caste ordination or literal war justification (Gandhian, universalist, non-dualist, materialist, Dalit-Bahujan movements) are structurally excluded from the orthodoxy's authority structure. They are present in the discourse but denied the interpretive standing that the Brahmin institutional seat grants to the orthodox reading. They can articulate alternative readings but lack the institutional power to make them authoritative within orthodox Hindu communities. Their voices would transform the constraint if heard and accepted, but the constraint's enforcement structure systematically excludes their testimony from having weight.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, dissident_philosophical_voices, excluded,
    moderate, biographical, constrained, regional).

% Modern reform movements (19th–21st centuries) critique the orthodox reading's caste and violence justifications while claiming fidelity to the text. They attempt to reframe the constraint without abandoning the kernel, producing the sibling readings. From the orthodoxy's seat, they are heretical challengers; from their own seats, they are recovering the text's deeper intent. They have sufficient institutional power to sustain alternative readings (schools, temples, networks) but not yet to overturn the orthodox interpretation in traditional communities.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, hindu_reform_movements, observer,
    organized, biographical, mobile, regional).

% Encountered the text and its Orthodox reading in colonial India and weaponized both: cited caste hierarchy as proof of Hindu barbarism justifying colonial rule, while selectively supporting the orthodoxy's Brahmin interpreters as intermediaries in the colonial administration. Their encounter altered the text's circulation and the context in which the constraint operates but did not fundamentally change the constraint's structure within orthodox Hindu communities. Their power was temporary; the constraint persists after their exit.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, colonial_british_authority, observer,
    institutional, biographical, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_priestly_class).
narrative_ontology:fixing_cost_class(gita_kurukshetra_discourse__orthodox_literal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a cosmological and ethical framework for maintaining social order through hereditary functional specialization (varna dharma). Coordinates role expectations across castes, reduces transaction costs of social organization by declaring roles divinely ordained rather than negotiable, and stabilizes the hierarchy by making deviation from one's caste duty a cosmic violation. This is a genuine coordination function: without some shared framework for role assignment, societies face constant conflict over who does what work and who claims authority.
% TRANSFER_FUNCTION: Extracts ritual authority, material support (taxation, labor service, land rights), and social deference from lower castes to Brahmin priestly interpreters and to the varna hierarchy itself. Transfers legitimate monopoly on violence to the Kshatriya warrior class in service of defending and expanding the hierarchy. Moves the burden of order-maintenance (military service, war casualties, manual labor, ritual pollution) downward to lower castes and women, while concentrating authority, honor, and material benefit upward to Brahmin interpreters and Kshatriya warriors.
% ABSENT_VOICES: Non-Brahmin philosophical schools (Lokayata materialists who would reject the text's authority entirely, Jain renouncers who would reject violence even in duty, Buddhist universalists who would reject caste as irrelevant to liberation), lower-caste voices who would testify that the hierarchy is constructed rather than cosmic and that their 'duty' operates as slavery, women across castes whose testimony about gender-caste double-bind is excluded, war victims themselves, and contemporary scientific and historical analysis showing that caste hierarchy is a human construction, not a cosmic order. The orthodox reading is promulgated almost exclusively by Brahmin interpreters and Kshatriya power-holders; alternative readings exist but are structurally outside the orthodoxy's authority circuit and are not granted equal standing in orthodox communities.
% DISAPPEARANCE_RATIONALE: The Gita text itself would remain, but if the orthodox literal reading ceased to be authoritative in Hindu communities—if the Brahmin interpretive monopoly broke, if kshatriya violence lost its dharmic license, if lower castes no longer accepted caste duty as cosmically mandated—the varna system would lose its theological anchor and become merely a power structure without legitimating narrative. Lower castes would have grounds to demand exit from subordination; warfare could not be framed as righteous duty but only as violence for power. The social order that the constraint sustains would face restructuring pressures it currently suppresses through the reading's authority.
% FOUNDING_PROBLEM: Early Vedic social organization (circa 1500–1200 BCE) required stable differentiation of labor and ritual roles across a multi-ethnic, multi-occupational population in North India. The text was authored to solve the coordination problem: how to make hereditary inequality feel inevitable and cosmically ordained rather than contested and imposed, and how to legitimate the violence required to maintain the hierarchy against resistance from those subordinated by it.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox Brahmin interpreters attest the founding problem is eternally live: cosmic order perpetually requires role stability and hierarchy is a natural expression of dharma that must be defended continuously. Modern historians (both Hindu and Western scholars) attest the problem was a real historical coordination challenge in Vedic times, but is no longer structurally necessary in modern societies with alternative coordination mechanisms (law, market exchange, bureaucratic specialization not tied to birth). Colonial administrators and Hindu reform movements (Gandhi, Ambedkar, Periyar, Phule) attest the hierarchy persists because the constraint's authority continues to operate, not because voluntary coordination requires it. No corroboration of the founding problem's continued necessity exists from lower castes themselves within the orthodox reading's authority structure—their testimony is excluded by the constraint itself. Modern social scientific evidence (historical demography, economic analysis, educational outcomes) shows caste hierarchy operates as pure asymmetric extraction, not as functional coordination.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__orthodox_literal_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__orthodox_literal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__orthodox_literal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gita_kurukshetra_discourse__orthodox_literal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__orthodox_literal_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the reading concentrates authority (interpretation), honor (warrior caste justification), and material benefit (Brahmin support) while diffusing burden (caste duty, war casualties) downward and locking lower castes in hereditary subordination without reciprocal benefit. Suppression is higher (0.87) because the constraint's persistence depends on active enforcement of interpretive monopoly—lower-caste challenges are structurally excluded, alternative readings are delegitimized, and the violence required to maintain the hierarchy is itself legitimated by the reading. Theater is moderate (0.41): the ritual and philosophical dimensions are genuine, but a growing share of enforcement is devoted to suppressing dissent rather than performing coordination. The temporal trajectory shows extractiveness and suppression rising slightly through the interval (t0-t25), with theater plateauing—a pattern consistent with constraint maturation under increasing challenge (reform movements, colonial disruption, modernization) that the orthodoxy responds to by hardening enforcement rather than conceding.
 *
 * PERSPECTIVAL GAP:
 *   From the Brahmin interpretive seat: the reading is a true and necessary expression of cosmic order, and their authority serves the whole hierarchy by maintaining dharmic knowledge. From the lower-caste seat: the same reading is a mechanism of structural exploitation—their subordination is presented as duty, their exit is framed as cosmic violation, and their testimony is excluded. The engine computes this divergence per-seat from the structural data (power differential, exit options, beneficiary/victim status); the authored claim does not adjudicate which seat is correct, only that they compute differently.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmin priestly class: d approaches 0.0 (full beneficiary)—they set the interpretive agenda, collect authority and material support, and face mobile exit (can reinterpret, can migrate). Kshatriya warrior class: d near 0.3 (beneficiary with constraints)—they gain moral license for violence but remain bound by the duty framework itself; warrior identity is partially identity-locked. Lower castes: d approaches 1.0 (full target)—they bear the cost of the hierarchy (extraction of labor, ritual disability, denial of alternatives), face identity-locked exit (born into the role), and have powerless status that amplifies suppression. War casualties: d = 1.0 (total target)—they are trapped and die in the conflict the reading legitimates. Women across castes: d near 0.85 (high target with slight beneficiary complexity)—they are constrained by both gender and caste, though elite women may derive some status from their caste position relative to lower-caste men; the constraint operates them as vectors of reproduction and service for the hierarchy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (early Vedic social coordination) is contested as to whether it remains live. Orthodox interpreters say yes (order perpetually requires hierarchy); modern analysts say the problem was solved and the constraint now persists as institutional inertia and power concentration. The constraint is NOT a piton (performance without function) because the beneficiary seats—Brahmin interpreters and Kshatriya power-holders—actively maintain it through enforcement of interpretive monopoly and exclusion of dissident voices. It is a tangled_rope: it solves a real coordination problem (stable role assignment) while extracting asymmetrically from those locked into subordinate roles. The mandatrophy is CONTESTED, not resolved—the question of whether the constraint's original function is still vital feeds directly into the sibling-reading competition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_literalism_ambiguity,
    'Is the text''s literal historical meaning what the orthodox reading claims (caste duty and righteous war), or does the text''s actual authorial intent diverge from what medieval commentators (Adi Shankara) and modern orthodox interpreters attribute to it?',
    'Philological analysis of the text''s composition history, Vedic-era social context, and comparison with non-Hindu and heterodox Hindu readings of the same passages. Historical reconstruction of what the text''s authors likely intended vs. what later interpreters claimed they intended.',
    'If the literal meaning diverges from the orthodox reading, then the reading is a later imposition, not a true interpretation—reclassifying the constraint from tangled_rope (genuine coordination) to snare (extraction under cover of false naturalness). If the orthodox reading is accurate to the text''s composition, the constraint remains tangled_rope but the mandate is clearer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_literalism_ambiguity, empirical, 'Whether the orthodox reading faithfully represents the text''s authorial intent or imposes later interpretive doctrines onto the text.').

omega_variable(
    caste_system_benignity_contested,
    'Is the varna system in the orthodox reading a system of functional coordination with reciprocal duties (varna-based mutual obligation), or is it a system of asymmetric extraction where lower castes bear costs without commensurate benefit?',
    'Historical examination of varna-system operation: do textual prescriptions for Brahmin and Kshatriya duties toward Shudras (feeding, protection, teaching within limits) actually operate as mutual obligation, or are they systematically violated while Shudra duties (service, taxes, submission) are enforced? Comparison with systems that claim reciprocity but operationally suppress lower-caste exit and voice.',
    'If the system operates as genuine reciprocal coordination, the constraint is a tangled_rope with some redistribution. If reciprocity is violated systematically, the constraint becomes a snare—extraction under the cover of a false coordination narrative. The orthodoxy claims mutual duty; the historical record shows systematic one-directional extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(caste_system_benignity_contested, empirical, 'Whether the varna hierarchy operates as reciprocal functional coordination or as asymmetric extraction with cosmological cover.').

omega_variable(
    dharmic_violence_universalizability,
    'When the orthodox reading legitimates ''righteous violence'' for Kshatriyas fulfilling caste duty, is this framing consistently applied to all castes, or is the permission to wage war systematically reserved to the dominant warrior caste?',
    'Historical examination of how the orthodoxy treats violence authorized by lower castes (peasant rebellions, dalit resistance, shudra armed dissent). Are they treated as legitimate dharmic duty, or as violation of caste duty?',
    'If dharmic violence is universalizable, the constraint grants legitimate agency to all castes—a less extractive distribution. If legitimacy for violence is reserved to Kshatriyas, the constraint is a mechanism of power monopoly. The historical record shows systematic delegitimization of lower-caste resistance as ''adharmic,'' suggesting the violence permission is not universalizable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dharmic_violence_universalizability, empirical, 'Whether dharmic violence is a universal permission or a caste-reserved monopoly on legitimacy.').

omega_variable(
    brahmin_interpretive_monopoly_necessity,
    'Is Brahmin priestly monopoly on scriptural interpretation a structural necessity for maintaining the text''s coherence and authority, or is it an institutional advantage claimed by Brahmin interpreters and enforced through suppression of non-Brahmin voices?',
    'Examine whether non-Brahmin interpretations of the Gita exist, whether they are coherent and grounded in the text, and whether they are excluded by logical necessity or by institutional power. Compare with the structure of textual authority in other traditions (Islamic tafsir, Christian exegesis) where non-clerical and non-credentialed voices sometimes participate.',
    'If the monopoly is structurally necessary, it is part of the coordination cost. If it is institutionally enforced, it is extraction—a way for Brahmins to preserve authority and material benefit by excluding rival voices. The evidence suggests enforcement rather than necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brahmin_interpretive_monopoly_necessity, conceptual, 'Whether Brahmin interpretive monopoly is structurally necessary for coherence or institutionally enforced for power concentration.').

omega_variable(
    identity_lock_mechanism_internalization_vs_structural,
    'How much of the lower-caste acceptance of caste duty derives from internalized identity-lock (the person believes subordination is their nature, duty, or dharma) vs. structural suppression (they would exit if external barriers dissolved)?',
    'Post-exit analysis: when lower castes migrate to urban environments, change professions, or join reform movements that reject caste, do they accept the orthodox reading, or do they actively abandon it? If internalized, the identity persists after exit; if structural, it dissolves.',
    'If internalized, the constraint''s suppression is higher than the scalar metric suggests—the target carries it with them. If structural, exit is possible and the constraint''s persistence depends on active enforcement, not on the victims'' own acceptance. The modern reform-movement data suggests significant structural component with some internalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_internalization_vs_structural, empirical, 'Whether lower-caste identity-lock is internalized (belief in dharma) or structural (enforced exclusion from alternatives).').

omega_variable(
    sibling_reading_foreclosure_vs_coexistence,
    'Do the three readings of the kernel (orthodox literal, Gandhian allegorical, universalist devotional) logically foreclose one another, or do they coexist as different readings held by different parties simultaneously?',
    'Examine whether a single reading can incorporate elements of the siblings (e.g., an orthodox interpreter who accepts allegorical elements, or a universalist who accepts literal dharma), or whether the readings are fundamentally incompatible premises. Assess the historical coexistence of these readings in Hindu communities.',
    'If forecloses: the orthodox reading is in zero-sum competition with siblings, and its institutional dominance actively suppresses alternatives. If coexists: the readings are different factions of an ongoing dispute, and suppression is enforcement of dominance within a contested field. The evidence suggests coexistence under institutional dominance—the readings compete but all remain live options.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_vs_coexistence, conceptual, 'Whether the sibling readings logically foreclose the orthodox reading or coexist as competing interpretations in the same tradition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__orthodox_literal_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(gita_tr_t5, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(gita_tr_t10, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(gita_tr_t15, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(gita_tr_t20, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(gita_tr_t25, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 0, 0.71).
narrative_ontology:measurement(gita_be_t5, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 5, 0.76).
narrative_ontology:measurement(gita_be_t10, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 10, 0.79).
narrative_ontology:measurement(gita_be_t15, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 15, 0.81).
narrative_ontology:measurement(gita_be_t20, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 20, 0.82).
narrative_ontology:measurement(gita_be_t25, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 25, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t0, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0, 0.81).
narrative_ontology:measurement(gita_su_t5, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 5, 0.83).
narrative_ontology:measurement(gita_su_t10, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(gita_su_t15, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 15, 0.86).
narrative_ontology:measurement(gita_su_t20, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 20, 0.87).
narrative_ontology:measurement(gita_su_t25, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 25, 0.87).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__orthodox_literal_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gita_kurukshetra_discourse__orthodox_literal_reading, 0.12).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse__gandhian_allegorical_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse__universalist_devotional_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_ritual_authority_monopoly).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, varna_system_hereditary_duty).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel gita_kurukshetra_discourse. The orthodx literal reading (this story) claims that the text mandates caste-based duty and legitimates righteous violence. Two sibling readings contest this by offering alternative interpretations: the Gandhian reading treats the battlefield as metaphor for internal struggle, rejecting literal war justification; the universalist reading emphasizes path-independent devotion and rejects caste as the source of dharma. Each reading instantiates a structurally distinct constraint (different ε, beneficiary/victim sets, suppression mechanisms) despite sharing the same kernel text. The three constraints are linked via this network field; see the omega variables for the structural relationships between readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
