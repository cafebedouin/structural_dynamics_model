% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__hereditary_monopoly_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_dharmic_corpus__hereditary_monopoly_reading, []).

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
 *   constraint_id: vedic_dharmic_corpus__hereditary_monopoly_reading
 *   human_readable: Vedic Dharmic Corpus – Hereditary Monopoly Reading
 *   domain: religious_authority/social_stratification/interpretive_legitimacy
 *
 * SUMMARY:
 *   The hereditary monopoly reading instantiates a constraint wherein ritual
 *   and interpretive authority over the Vedic corpus derives exclusively from
 *   birth into Brahmin lineage. The varna hierarchy (caste system) is treated
 *   as divinely ordained through the Vedas themselves and textually
 *   prescribed as eternal cosmic order. This reading structures authority so
 *   that lower castes, women, and untouchables are systematically excluded
 *   from access to Vedic knowledge, priestly function, and religious
 *   interpretation—they require Brahmin intermediaries for ritual
 *   participation. The constraint persists through multiple mechanisms:
 *   textual authority (Vedas prescribe the system), institutional enforcement
 *   (temples and educational structures controlled by Brahmins),
 *   identity-lock (being Brahmin is constitutive of priestly identity), and
 *   suppression of alternative readings (treating bhakti movements and
 *   reformist interpretations as heterodox). This is ONE reading of the
 *   contested Vedic dharmic kernel. The bhakti devotional reading proposes
 *   direct devotional access bypassing caste; the reformist egalitarian
 *   reading subsumes caste hierarchy as historical accretion rather than
 *   scriptural essence and subjects interpretation to constitutional equality
 *   principles.
 *
 * KEY AGENTS:
 *   - Brahmin priestly class: institutional agenda-setter, benefits from exclusive interpretive and ritual authority, identity-fused with priestly function
 *   - Lower castes (shudra, vaishya): powerless payers, excluded from Vedic knowledge, require Brahmin intermediaries, trapped in ritual dependency
 *   - Women (especially non-Brahmin women): powerless payers, excluded from Vedic study, identity-locked through household dharma relations
 *   - Untouchables: powerless payers, exterior to varna hierarchy, ritually polluting by definition, maximum exclusion
 *   - Warrior and merchant castes (kshatriya, vaishya): moderate power but denied interpretive authority, excluded from religious legitimacy despite some domain authority
 *   - Bhakti reform movements: organized excluded party, propose alternative reading granting devotional access, gain followings among lower castes and women
 *   - Constitutional framers (postcolonial): institutional observers, establish equality framework directly contradicting hereditary monopoly reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.68).
domain_priors:suppression_score(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.79).
domain_priors:theater_ratio(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__hereditary_monopoly_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__hereditary_monopoly_reading, "Vedic Dharmic Corpus – Hereditary Monopoly Reading").
narrative_ontology:topic_domain(vedic_dharmic_corpus__hereditary_monopoly_reading, "religious_authority/social_stratification/interpretive_legitimacy").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__hereditary_monopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__hereditary_monopoly_reading, 'b8536ab0-e620-427c-ad6f-1a7e24332c87').
narrative_ontology:cs_kernel_codification('b8536ab0-e620-427c-ad6f-1a7e24332c87', fixed_text).
narrative_ontology:cs_authority_grounding('b8536ab0-e620-427c-ad6f-1a7e24332c87', lineage).
narrative_ontology:cs_interpretation_layer_present('b8536ab0-e620-427c-ad6f-1a7e24332c87').
narrative_ontology:cs_reading_relation('b8536ab0-e620-427c-ad6f-1a7e24332c87', vedic_dharmic_corpus__bhakti_devotional_reading, coexists_with).
narrative_ontology:cs_reading_relation('b8536ab0-e620-427c-ad6f-1a7e24332c87', vedic_dharmic_corpus__reformist_egalitarian_reading, coexists_with).
narrative_ontology:cs_axiom('b8536ab0-e620-427c-ad6f-1a7e24332c87', foundational, ritual_authority_from_vedic_birth_transmission).
narrative_ontology:cs_axiom_status(ritual_authority_from_vedic_birth_transmission, holdable).
narrative_ontology:cs_axiom_grounding('b8536ab0-e620-427c-ad6f-1a7e24332c87', ritual_authority_from_vedic_birth_transmission, conventional).
narrative_ontology:cs_axiom('b8536ab0-e620-427c-ad6f-1a7e24332c87', foundational, varna_hierarchy_divinely_ordained_and_textually_prescribed).
narrative_ontology:cs_axiom_status(varna_hierarchy_divinely_ordained_and_textually_prescribed, holdable).
narrative_ontology:cs_axiom_grounding('b8536ab0-e620-427c-ad6f-1a7e24332c87', varna_hierarchy_divinely_ordained_and_textually_prescribed, theological).
narrative_ontology:cs_reference_frame('b8536ab0-e620-427c-ad6f-1a7e24332c87', vedic_dharmic_eternal_order).
narrative_ontology:cs_drift_state('b8536ab0-e620-427c-ad6f-1a7e24332c87', contemporary_constitutional_equality, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b8536ab0-e620-427c-ad6f-1a7e24332c87', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, lower_castes).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, women).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, untouchables).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Birth into Brahmin lineage confers exclusive right to perform Vedic rituals, interpret sacred texts, and adjudicate dharmic questions. Controls access to ritual economy (temples, patronage networks, educational transmission of Vedic knowledge). Justifies monopoly as necessary consequence of ritual purity and textual knowledge accessible only through generational transmission. Personal identity is inseparable from priestly function and authority; exit would mean renouncing Brahmin identity itself.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class, agenda_setter,
    institutional, civilizational, identity_locked, continental).

% Excluded from learning or performing Vedic rituals; required to seek Brahmin intermediaries for religious services and authority. Pay through patronage obligations, labor services (historically; contemporary: ritual fees), and deference to Brahmin interpretive authority on dharmic matters. Have no legitimate alternative authority structure; exit from the constraint means renouncing participation in the dominant religious framework entirely, a civilizational-scale exit cost.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, lower_castes, payer,
    powerless, civilizational, trapped, continental).

% Excluded from vedic study and ritual performance, classified in texts as permanent wards requiring male authority (father, husband, son). Even women of Brahmin birth cannot access the priestly function. Participate through household dharma (domestic ritual, obedience) mediated by male kin. Identity is constitutively relational (daughter/wife/widow); autonomy to exit the constraint requires rejecting family and social personhood simultaneously.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, women, payer,
    powerless, civilizational, identity_locked, continental).

% Structurally exterior to varna hierarchy; engaged in ritually polluting labor (leather work, corpse disposal). Not merely excluded from Brahmin authority but declared ritually contaminating by contact. Have no legitimate religious authority claim whatsoever; even lower castes outrank them. Exit requires geographic and social migration away from varna-organized communities.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, untouchables, payer,
    powerless, civilizational, trapped, continental).

% Occupy varna positions (kshatriya, vaishya, shudra) with some legitimate authority in their domains (kingship, commerce, service), but interpretive religious authority is denied them. Could articulate alternative readings granting them access to Vedic authority, but textual and institutional structures prevent them from being heard as legitimate interpreters. Their exclusion is structural, not accidental.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, warrior_merchant_laborer_castes, excluded,
    moderate, civilizational, constrained, continental).

% Propose alternative readings (direct devotional access, sincere bhakti over caste birth) that would dissolve the Brahmin monopoly. Historically gain followings, particularly among lower castes and women. Treated by hereditary monopoly reading as heterodox, sectarian, spiritually inferior—their access to legitimacy is controlled by the institutions they seek to displace.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, bhakti_reform_movements, excluded,
    organized, biographical, mobile, continental).

% Document and initially codify caste hierarchy through census and law, treating varna classifications as administrative categories. Later observers (postcolonial constitutional authority, reformers) challenge the constraint from outside the traditional legitimacy framework.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, colonial_administrators, observer,
    institutional, biographical, analytical, continental).

% Author equality provisions that directly contradict the constraint's core claim (birth-determined privilege). Do not control the constraint's operation but establish an alternative authority framework whose legitimacy fundamentally conflicts with the hereditary monopoly reading.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, constitutional_framers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class).
narrative_ontology:fixing_cost_class(vedic_dharmic_corpus__hereditary_monopoly_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes ritual performance, textual interpretation, and spiritual authority around a stable, generationally transmitted priesthood. Prevents free-rider appropriation of ritual authority (anyone claiming Vedic legitimacy without validation). Provides consistent, hierarchical adjudication of dharmic questions and religious practice across a vast, diverse population.
% TRANSFER_FUNCTION: Moves ritual authority, interpretive legitimacy, and spiritual resources from the general population to Brahmin lineage exclusively. Extracts labor, patronage, and deference from lower castes and women who require Brahmin intermediaries for religious participation. The extraction is justified as the price of ritual purity and textual knowledge accessibility.
% ABSENT_VOICES: Lower castes, women, and untouchables would object to the entire structural claim (that ritual authority derives from birth rather than knowledge, devotion, or character); bhakti reform movements object that sincere devotion should bypass caste; constitutional framers object that equality trumps traditional lineage claims. Their exclusion is built into the legitimacy structure—they are deemed unfit to interpret the texts that prescribe their own subordination.
% DISAPPEARANCE_RATIONALE: If the hereditary monopoly reading and its enforcement vanished overnight, ritual authority would become contestable—lower-caste and bhakti practitioners would claim legitimacy, women would enter priestly functions, interpretive authority would disperse across reformist and egalitarian readings. The entire social-spiritual hierarchy would reorganize around alternative legitimacy claims (character, knowledge, devotion, constitutional equality). The Brahmin priestly class would lose its civilizational-scale monopoly on religious authority and the economic/social power that monopoly sustains.
% FOUNDING_PROBLEM: Early Vedic ritual system was complex and required trained specialists to preserve knowledge and ensure correct performance. Brahmin lineages developed as custodians of this knowledge through generational transmission. Varna hierarchy provided a way to organize society such that Brahmins could dedicate themselves to ritual mastery without competing for other productive labor.
% FOUNDING_PROBLEM_CORROBORATION: Brahmin traditionalists attest the founding problem is still live: Vedic knowledge is arcane and requires lifelong immersion; the hereditary transmission system is the only mechanism that preserved the texts across millennia. Lower-caste movements and constitutional authorities attest the founding problem is solved or was never real: knowledge can be transmitted through any capable student regardless of birth; the problem the hereditary system solves is not preservation but monopoly-maintenance. Historical scholarship supports that the problem (preserving complex ritual) was real in early periods; comparative religious evidence shows no structural reason knowledge transmission requires birth restriction—it is a choice, not a necessity.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__hereditary_monopoly_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__hereditary_monopoly_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__hereditary_monopoly_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vedic_dharmic_corpus__hereditary_monopoly_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__hereditary_monopoly_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vedic_dharmic_corpus__hereditary_monopoly_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vedic_dharmic_corpus__hereditary_monopoly_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end, rising from 0.58 at start) because the constraint delivers exclusive benefit to one class while imposing systematic costs on all others: Brahmins monopolize ritual authority and economic patronage networks; lower castes pay through deference, labor, and ritual fees; women are excluded from autonomous religious authority; untouchables are treated as ritually contaminating. The trajectory shows slight accumulation (0.58→0.68 over 25 intervals), consistent with institutional hardening and increasing systematization of enforcement under colonial and postcolonial periods. Suppression is high and stable (0.75–0.79) because the constraint depends on active enforcement through textual interpretation (declaring competing readings heterodox), institutional control (temple gatekeeping), and suppression of alternative authority claims (treating lower-caste and women's religious authority as impossible). Theater ratio rises from 0.25 to 0.42 (a 68% increase), indicating that as actual resistance to the constraint grows (particularly from reformist and constitutional movements), more enforcement activity becomes performative—defending the legitimacy of the system rather than the coordination it was founded for. The measurement grid is synchronized: every metric is authored at every time point across the shared interval [0, 25]. The interval represents the period from early postcolonial codification of the constraint (as colonial census and law reified caste) through contemporary contestation by constitutional equality frameworks and continuing bhakti-reform movements.
 *
 * PERSPECTIVAL GAP:
 *   Brahmin institutional seat vs. lower-caste/powerless seats. Brahmins are the agenda-setter: they define the constraint, interpret the texts, control the enforcement machinery, and collect the benefits. Their temporal horizon is civilizational (the constraint has existed in recognizable form for ~2,500 years of continuous Brahmin priestly lineages). Their exit options include arbitrage (delegating to other Brahmins, navigating within the knowledge transmission system). From their seat, the constraint solves a real problem (organizing complex ritual knowledge) and delivers unambiguous benefits (authority, patronage, social standing). They should compute the constraint as rope-like (coordination with clear beneficiaries) or as their own version of tangled_rope where they are the beneficiary seat and others are payers. Lower-caste seats are powerless, trapped, and have no domain-specific alternative authority. The same constraint that serves as coordinating infrastructure (from Brahmin perspective) operates on them as pure extraction: they have no access to the authority system, must pay for intermediaries, and have no exit except renouncing the entire framework. The engine should compute their seat as snare or snare-equivalent (pure extraction, active enforcement to prevent exit, no beneficiary role). The gap between these seat-computations is structural, not author-error: the constraint genuinely delivers coordination benefits to one class and pure extraction to others. This is the definition of tangled_rope at the story level: the same structural arrangement benefits some through genuine coordination and others through coercive extraction. The measuring apart of these computations is the analytical point.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmin priestly class: low directionality (d~0.15–0.20). Beneficiaries of the constraint's operation; institutional power; exit options are arbitrage-grade (can delegate to other Brahmins, can shift interpretive authority within the class, have alternative authority domains). The constraint subsidizes their position—it delivers monopoly rents and exclusive legitimacy. From their seat the constraint is mostly coordinating (preserving knowledge, organizing ritual). Lower castes and untouchables: high directionality (d~0.85–0.95). Trapped powerless agents; exit from the constraint means leaving the entire religious framework and associated community structures. The constraint extracts from them: they cannot participate in the authority system that governs their own spiritual life. Women: high directionality (d~0.80–0.90). Identity-locked through family structures (father/husband/son authority); Vedic participation is mediated through male kin or renounced entirely (celibate ascetic path). Even Brahmin women are excluded from the priestly function itself. Warrior and merchant castes: moderate directionality (d~0.50–0.60). Have legitimate authority domains (kingship, commerce) but religious/interpretive authority is denied; the constraint constrains but does not trap them because they have alternative power bases. Bhakti movements: moderate-to-high directionality (d~0.60–0.75) in their historical periods because they must operate against the institutional exclusion; organized power enables some mobility. Constitutional framework observers: analytical directionality (d=0.5 or excluded from directionality computation as non-seats). The beneficiary/victim structure is stark: one clear beneficiary class (Brahmins) and three overlapping victim classes (lower castes, women, untouchables). Suppression is not scaled in directionality computation (structural raw property); extractiveness is scaled by directionality and scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The hereditary monopoly reading exhibits clear mandatrophy: the founding problem (preserving complex Vedic knowledge through generational transmission) is substantially solved—literacy, printing, scholarly traditions, modern education all provide preservation mechanisms superior to birth-based restriction. The constraint persists not to solve the founding problem but to maintain the Brahmin monopoly. This is mandatrophy as textbook definition: the arrangement's original mandate is dead but the arrangement persists because beneficiary institutions benefit from suppressing the alternative. The measurement trajectory shows this: extractiveness rises slightly (0.58→0.68) while theater increases substantially (0.25→0.42), indicating that enforcement activity is increasingly defending the constraint's legitimacy rather than preserving knowledge. If the founding problem were live (knowledge preservation were genuinely threatened), suppression and extraction would remain stable and focused on gatekeeping. Instead, suppression remains high but increasingly devoted to preventing alternative readings (reform, egalitarian, bhakti) from gaining legitimacy. The constraint's persistence depends on identity-lock (Brahmin priestly identity fused with authority) and suppression (treating lower-caste and women's authority claims as impossible). The constraint does NOT persist because coordination is still needed—it persists because institutional inertia, identity stakes, and suppression machinery keep it functioning despite the founding mandate being satisfied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_vs_discovered_fact,
    'Is the hereditary monopoly reading the reading of a stable kernel (texts that require interpretation), or does it claim to be a discovered fact about what the Vedic texts objectively mean?',
    'Textual analysis: compare the Vedic texts directly against: (1) the monopoly reading''s interpretation (birth determines authority, varna is eternal cosmic order), (2) the bhakti reading''s interpretation (devotion transcends caste), (3) the egalitarian reading''s interpretation (caste is historical accretion not scriptural essence). Examine whether the texts are genuinely ambiguous or whether one reading more closely matches textual frequency and context.',
    'If the texts are genuinely ambiguous, the hereditary monopoly reading is a reading and benefits from transparency about competing readings. If the monopoly reading claims to be the objective textual meaning while others are heterodox interpretations, the authority grounding is shifted: it is extraction grounded in interpretive monopoly, not textual objectivity. This affects whether the constraint should be reclassified as snare (pure extraction grounded in illegitimate authority claim) rather than tangled_rope (hybrid coordination + extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_vs_discovered_fact, conceptual, 'Whether this is a reading of an ambiguous kernel or a claim to objective textual meaning that forecloses alternatives').

omega_variable(
    knowledge_preservation_vs_monopoly_maintenance,
    'Is the Brahmin priestly gatekeeping mechanism necessary to preserve Vedic knowledge and ensure correct ritual performance, or is knowledge preservation separable from the birth-restriction principle?',
    'Comparative historical and institutional analysis: (1) examine periods where knowledge transmission occurred despite lower-caste participation (bhakti scholars, reform movements); (2) examine knowledge preservation mechanisms outside varna-based priesthoods (Buddhist sangha, Islamic scholarship); (3) examine what actually happened to Vedic knowledge when gatekeeping failed (printing, colonial documentation, modern scholarship).',
    'If knowledge preservation is separable from birth restriction, the founding problem (preserving complex knowledge) can be solved without the extraction mechanism. This would establish the constraint as mandatrophy with clear resolution path: transformation to egalitarian or bhakti reading. If birth restriction is structurally necessary, the founding problem remains live and the constraint''s classification would be more rope-like despite its extractive mechanics. High-confidence resolution would force reclassification or mandate documentation of why non-caste-based alternatives fail.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(knowledge_preservation_vs_monopoly_maintenance, empirical, 'Whether the birth-restriction principle is necessary to the coordination function or instrumentally replaceable').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.79) structural (lower castes and women are prevented from learning texts, accessing temples, claiming authority) or internalized (victims have accepted the hereditary monopoly reading''s legitimacy frame and inhibit themselves from claiming authority)?',
    'Post-exit trajectory analysis: examine what happens when victims of the constraint gain access to alternative authority structures and frameworks (constitutional citizenship, bhakti communities, reform education). If suppression drops when structural barriers are removed, suppression is structural. If victims continue to inhibit authority claims even after barriers fall, suppression is internalized. Examine the trajectory of lower-caste and women''s participation in ritual authority when institutional gatekeeping weakens (contemporary temples with lower-caste priests, women''s ritual innovation in reform and bhakti movements).',
    'If suppression is structural, the constraint persists because institutions actively prevent alternative authority. If suppression is substantially internalized (beliefs about who ''should'' have authority), the constraint persists through both institutional barriers and cognitive capture. Internalized suppression requires longer-term intervention (education, identity reconstruction) to dissolve. This affects the predicted extinction trajectory and the cost of fixing the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is externally imposed or victim-internalized cognitive patterns').

omega_variable(
    committer_alternative_readings_foreclosure,
    'Does the hereditary monopoly reading logically foreclose the bhakti devotional reading within the same legitimacy framework, or do these readings coexist as competing live positions?',
    'Logical analysis of core axioms: the monopoly reading asserts (1) ritual authority requires Vedic knowledge, (2) Vedic knowledge derives from Brahmin birth-transmission, (3) therefore non-Brahmins cannot have ritual authority. The bhakti reading asserts (1) direct devotion bypasses textual mediation, (2) sincere bhakti is spiritual authority sufficient, (3) therefore birth does not determine spiritual access. These axioms directly contradict IF both readings claim that Vedic knowledge/textual authority is the measure of spiritual legitimacy. But the bhakti reading denies that premise—it substitutes devotion for knowledge as the measure. Do these readings coexist (different parties hold them simultaneously) or does one strictly entail the falsity of the other?',
    'If the readings foreclose each other (one entails the other is false in any single framework), they represent genuine kernel-level contradiction. If they coexist as competing live positions, the kernel''s legitimacy is genuinely contested and both readings remain viable. This affects the reading_relations declaration in cs_structure: coexists_with vs. forecloses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_alternative_readings_foreclosure, conceptual, 'Whether the monopoly reading logically rules out bhakti reading or both remain simultaneously holdable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__hereditary_monopoly_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(vedi_tr_t5, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(vedi_tr_t10, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(vedi_tr_t15, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(vedi_tr_t20, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(vedi_tr_t25, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(vedi_be_t5, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 5, 0.61).
narrative_ontology:measurement(vedi_be_t10, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 10, 0.64).
narrative_ontology:measurement(vedi_be_t15, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 15, 0.67).
narrative_ontology:measurement(vedi_be_t20, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(vedi_be_t25, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(vedi_su_t5, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 5, 0.76).
narrative_ontology:measurement(vedi_su_t10, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 10, 0.77).
narrative_ontology:measurement(vedi_su_t15, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 15, 0.78).
narrative_ontology:measurement(vedi_su_t20, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 20, 0.79).
narrative_ontology:measurement(vedi_su_t25, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 25, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__hereditary_monopoly_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.12).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus__bhakti_devotional_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus__reformist_egalitarian_reading).

% DUAL FORMULATION NOTE:
% This is one of three readings of the vedic_dharmic_corpus kernel. The hereditary monopoly reading treats varna hierarchy as divinely ordained and birth-determined. The bhakti devotional reading proposes direct devotion bypasses caste. The reformist egalitarian reading subsumes caste as historical accretion subject to constitutional equality reinterpretation. Each reading has different epsilon (extractiveness), different beneficiaries, different institutional enforcement mechanisms. They share a kernel (the Vedic texts treated as authoritative) but instantiate different constraints. All three readings are linked via network.affects_constraints so the corpus can track how they compete for institutional legitimacy and how reformation/constitutional authority displaces the hereditary monopoly reading over time.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
