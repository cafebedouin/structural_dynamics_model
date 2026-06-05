% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__hereditary_monopoly_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
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
 *   constraint_id: vedic_dharmic_corpus__hereditary_monopoly_reading
 *   human_readable: Vedic Dharmic Corpus: Hereditary Monopoly Reading
 *   domain: religious_authority/social_stratification/interpretive_legitimacy
 *
 * SUMMARY:
 *   This constraint story instantiates the HEREDITARY MONOPOLY READING of the
 *   contested Vedic Dharmic Corpus kernel. It describes the institutionalized
 *   claim that ritual and interpretive authority over Vedic texts derive
 *   exclusively from hereditary Brahmin birth, grounded in divine ordination
 *   and perpetuated through the varna hierarchy. This is ONE specific reading
 *   of how Vedic authority is legitimated — not the only possible reading,
 *   and historically contested by bhakti devotional movements (which claim
 *   direct divine access without priestly intermediary) and by
 *   reformist/egalitarian movements (which assert that Vedic knowledge is
 *   learnable independent of birth status). The constraint exhibits high
 *   extractiveness (ε=0.68) because the hereditary monopoly creates clear and
 *   substantial asymmetries: Brahmin priestly class captures ritual
 *   authority, material resources from temple economies, and epistemic
 *   privilege; lower castes and women bear suppression through exclusion,
 *   labor obligation, and ritual impurity prescriptions. The theater ratio
 *   (0.65) reflects that while actual ritual coordination is required
 *   (temples do aggregate labor, maintain calendars), a significant portion
 *   of the constraint's perpetuation is performative — rhetorical assertion
 *   of divine ordination, cultural authority claims, and purity theater that
 *   exceed functional necessity. Suppression has intensified over the
 *   classical consolidation period (t=5) from 0.65 to 0.78 as the hierarchy
 *   was systematized through texts like the Manusmrti and as temple economies
 *   became institutionalized. The constraint is a snare from the powerless
 *   perspective (trapped lower castes with no exit), ropelike from the
 *   institutional Brahmin perspective (sees pure coordination), and tangled
 *   rope from intermediate castes and institutional enforcement perspectives
 *   (experience both coordination benefit and extraction cost). The
 *   analytical perspective risks naturalizing this as immutable (mountain
 *   framing) but the structural data reveals it as a false summit: hereditary
 *   monopoly is a specific institutional reading, not a natural law.
 *
 * KEY AGENTS:
 *   - Brahmin Priestly Class: Institutional beneficiary (institutional/arbitrage) — holds monopoly on Vedic interpretation, ritual performance, and temple authority; captures resources and social prestige
 *   - Lower Castes (Shudra, Vaisya): Primary victims (powerless/trapped) — excluded from Vedic study, assigned ritual impurity, locked into labor obligations, no exit capacity
 *   - Women: Secondary victims (powerless/identity_locked) — structurally mobile but identity-fused with dharmic duty; epistemically excluded from Vedic learning; extraction reinforced through internalized virtue norms
 *   - Merchant and Warrior Castes (Vaishya, Kshatriya): Intermediate actors (organized/constrained) — positioned between Brahmin authority and lower-caste labor; benefit from caste hierarchy but pay extraction costs to Brahmin priestly class
 *   - Temple Economics System: Institutional enforcement mechanism (institutional/constrained) — aggregates ritual labor and resources; enforces caste compliance through ritual access control and resource redistribution
 *   - Post-Colonial Brahmin Institutions: Institutional degradation (institutional/arbitrage) — Vedic hereditary authority claims persist through cultural inertia and identity fusion despite loss of enforcement machinery (British law, secular education)
 *   - Analytical Observer: Civilization-level view (analytical/analytical) — risks naturalizing contingent institutional arrangements as immutable features of how societies organize sacred authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.68).
domain_priors:suppression_score(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.78).
domain_priors:theater_ratio(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__hereditary_monopoly_reading, snare).
narrative_ontology:human_readable(vedic_dharmic_corpus__hereditary_monopoly_reading, "Vedic Dharmic Corpus: Hereditary Monopoly Reading").
narrative_ontology:topic_domain(vedic_dharmic_corpus__hereditary_monopoly_reading, "religious_authority/social_stratification/interpretive_legitimacy").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__hereditary_monopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__hereditary_monopoly_reading, '50c82304-ee17-4b5d-a7dc-a2b9d0321fe2').
narrative_ontology:cs_kernel_codification('50c82304-ee17-4b5d-a7dc-a2b9d0321fe2', fixed_text).
narrative_ontology:cs_authority_grounding('50c82304-ee17-4b5d-a7dc-a2b9d0321fe2', lineage).
narrative_ontology:cs_interpretation_layer_present('50c82304-ee17-4b5d-a7dc-a2b9d0321fe2').
narrative_ontology:cs_reading_relation('50c82304-ee17-4b5d-a7dc-a2b9d0321fe2', vedic_dharmic_corpus__bhakti_devotional_reading, coexists_with).
narrative_ontology:cs_reading_relation('50c82304-ee17-4b5d-a7dc-a2b9d0321fe2', vedic_dharmic_corpus__reformist_egalitarian_reading, influences).
narrative_ontology:cs_axiom('50c82304-ee17-4b5d-a7dc-a2b9d0321fe2', foundational, hereditary_ritual_authority_necessity).
narrative_ontology:cs_axiom_status(hereditary_ritual_authority_necessity, holdable).
narrative_ontology:cs_axiom_grounding('50c82304-ee17-4b5d-a7dc-a2b9d0321fe2', hereditary_ritual_authority_necessity, deontological).
narrative_ontology:cs_axiom('50c82304-ee17-4b5d-a7dc-a2b9d0321fe2', foundational, birth_status_epistemic_qualification).
narrative_ontology:cs_axiom_status(birth_status_epistemic_qualification, overridden).
narrative_ontology:cs_axiom_grounding('50c82304-ee17-4b5d-a7dc-a2b9d0321fe2', birth_status_epistemic_qualification, deontological).
narrative_ontology:cs_reference_frame('50c82304-ee17-4b5d-a7dc-a2b9d0321fe2', hereditary_vedic_transmission).
narrative_ontology:cs_drift_state('50c82304-ee17-4b5d-a7dc-a2b9d0321fe2', post_colonial_contemporary, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('50c82304-ee17-4b5d-a7dc-a2b9d0321fe2', '2026-02-27T00:00:00Z').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, lower_castes).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, women).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, shudra_labor).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOWER CASTES / SHUDRA POPULATIONS (SNARE) — Structurally locked into ritual impurity, labor dependency, and exclusion from Vedic knowledge. Birth determines occupation and religious status without exit. Maximum extraction: labor and deference extracted; no epistemic standing to challenge authority. Trapped by caste law and ritual prohibition.
constraint_indexing:constraint_classification(vedic_dharmic_corpus__hereditary_monopoly_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: WOMEN (IDENTITY-LOCKED / SNARE) — Structurally mobile (could exit ritual roles materially) but identity-fused with dharmic duty and household compliance. Vedic authority prescribes female virtue as obedience; exiting this identity would require abandoning the normative frame that constitutes their social legitimacy. Epistemically excluded from Vedic study and ritual authority. Extraction runs high; exit capacity is real but cognitive capture prevents recognition.
constraint_indexing:constraint_classification(vedic_dharmic_corpus__hereditary_monopoly_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 3: BRAHMIN PRIESTLY CLASS (ROPE) — Experiences the constraint as pure coordination: the Vedic authority structure enables priestly knowledge transmission, ritual performance, and social authority. This perspective classifies as Rope because from the beneficiary's vantage point, the constraint solves a coordination problem (who has legitimate authority to perform rituals?) with no apparent extraction. The asymmetric power flows appear natural to their structural position.
constraint_indexing:constraint_classification(vedic_dharmic_corpus__hereditary_monopoly_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: MERCHANT/WARRIOR CASTES (TANGLED ROPE) — Vaishya and Kshatriya castes occupy intermediate positions: structurally included in the Vedic order (can study limited texts, perform some rituals) but subordinate to Brahmin priestly monopoly. They benefit from caste hierarchy (extraction flows downward from lower castes) but pay extraction costs (priestly fees, ritual dependency, exclusion from highest epistemic authority). Organized resistance emerges here — merchant guilds and warrior courts challenge Brahmin knowledge monopoly while maintaining the basic caste structure.
constraint_indexing:constraint_classification(vedic_dharmic_corpus__hereditary_monopoly_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: TEMPLE AND RITUAL ECONOMY (TANGLED ROPE) — The temple system is simultaneously a coordination mechanism (aggregates labor for public festivals, maintains ritual calendars) and an extraction apparatus (concentrates resources, redistributes through priestly patronage, enforces caste compliance through ritual access control). Theater is moderate — actual ritual function coexists with substantial performative hierarchy maintenance.
constraint_indexing:constraint_classification(vedic_dharmic_corpus__hereditary_monopoly_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: POST-COLONIAL BRAHMIN INSTITUTIONAL IDENTITY (PITON) — After colonialism, Brahmin authority lost its enforcement machinery (British law supplanted caste law; education became secular). Contemporary Brahmin institutional actors claim Vedic hereditary authority rhetorically while functioning through modern credentials, education, and civil service. The Vedic text-based legitimacy persists through inertia and identity fusion (Brahmin identity fused with knowledge transmission), but its functional extraction capacity has degraded. Theater is high — cultural authority and social prestige without institutional enforcement infrastructure.
constraint_indexing:constraint_classification(vedic_dharmic_corpus__hereditary_monopoly_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some hierarchy and ritual specialization may appear inevitable: complex societies require authority structures for coordinating ritual and knowledge transmission. This perspective risks naturalizing the hereditary monopoly as an immutable feature of how societies organize sacred authority. However, the structural data contradicts the mountain classification: clear beneficiaries (Brahmin priestly class), clear victims (lower castes, women), and institutional enforcement mechanisms indicate this is a constructed constraint, not a natural law. False summit candidate.
constraint_indexing:constraint_classification(vedic_dharmic_corpus__hereditary_monopoly_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__hereditary_monopoly_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(vedic_dharmic_corpus__hereditary_monopoly_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(vedic_dharmic_corpus__hereditary_monopoly_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(vedic_dharmic_corpus__hereditary_monopoly_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(vedic_dharmic_corpus__hereditary_monopoly_reading, TR),
    TR >= 0.70.

:- end_tests(vedic_dharmic_corpus__hereditary_monopoly_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.68): High. The constraint creates clear asymmetries in authority, knowledge access, ritual participation, and resource flow. Brahmin class extracts: ritual authority (monopoly on Vedic interpretation and performance), material resources (temple patronage and priestly fees), and social status (hierarchy legitimacy). Lower castes and women bear: exclusion from knowledge, labor obligation (corvée, untouchability restrictions), and suppression of agency. The value (0.68 rather than higher) reflects that some legitimate coordination function exists (temples do organize ritual, maintain calendars, aggregate community resources) — the constraint is not pure extraction but mixing coordination and extraction (tangled_rope from some perspectives). If this were pure extraction with no coordination, ε would approach 0.85+. Suppression (0.78): High. The constraint employs multiple suppressive mechanisms: (1) epistemological suppression — exclusion from Vedic study, denial of hermeneutic authority; (2) legal suppression — caste law codes (Manusmrti) prescribe occupations and ritual status by birth; (3) ritual suppression — pollution rules, untouchability restrictions prevent interaction and mobility; (4) identity suppression — dharmic duty ideology makes exit psychologically unthinkable for internalized actors. The progression from 0.65 (early Vedic period, more customary) to 0.78 (classical consolidation, more codified) reflects systematization of suppression through textual authority and temple institutions. Theater ratio (0.65): Moderate-high. Actual coordination (ritual performance, calendar maintenance, community gathering) is mixed with substantial performative activity (assertion of divine ordination, purity theater, status hierarchy maintenance). The theater is functional — it maintains the hierarchy and justifies extraction — but it exceeds the minimal functional requirement. If theater dropped below ~0.40, the constraint would degrade toward rope classification (pure coordination). Claimed type (Snare): Appropriate for this reading from the analytical perspective. High extractiveness, high suppression, and asymmetric power flows from beneficiary to victim match the snare signature. The beneficiary (Brahmin class) experiences it as rope (coordination), but that perspectival disagreement is exactly what the indexical system captures — different observers with different structural positions classify identically.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximal perspectival disagreement. The Brahmin beneficiary classifies it as Rope (pure coordination: 'we maintain sacred authority and ritual order'). The lower-caste victim classifies it as Snare (pure extraction: 'we are locked in by birth and excluded from knowledge'). The intermediate castes classify it as Tangled Rope (mixed coordination and extraction: 'we benefit from hierarchy relative to others, but we pay extraction costs to Brahmins'). The analytical observer risks Mountain classification (natural law: 'all societies need ritual authority and hierarchy') but the structural data (clear beneficiaries, clear victims, institutional enforcement, rising theater) indicates false summit — the constraint is constructed, not natural. The gap reveals the indexical dependency: whether this constraint is extractive or coordinative depends entirely on the observer's structural position within it. The same institutional phenomenon appears as legitimacy (rope) from inside the beneficiary's position, as oppression (snare) from the powerless position, as asymmetric coordination (tangled_rope) from intermediate positions, as necessary structure (mountain, false) from civilizational distance. This is the core diagnostic insight of indexical classification: no single universal perspective exists.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) and effective extractiveness (χ = ε × f(d) × σ(S)) are computed from each agent's structural relationship to the constraint. Brahmin priestly class: beneficiary + institutional power + arbitrage exit → d ≈ 0.05-0.15 → f(d) ≈ -0.10 to 0.02 → negative or near-zero χ. They experience the constraint as coordination or minimal extraction because they structurally benefit. Lower castes: victim + powerless + trapped → d ≈ 0.95 → f(d) ≈ 1.42 → χ ≈ 0.68 × 1.42 × 0.9 ≈ 0.87. They experience maximum extraction because they are targets with no exit. Women: victim + powerless + identity_locked → d ≈ 0.89 → f(d) ≈ 1.28 → χ ≈ 0.68 × 1.28 × 0.9 ≈ 0.79. They experience high extraction despite structural mobility because their identity is fused with the constraint; exiting would require abandoning the dharmic identity frame. Vaishya/Kshatriya castes: partial beneficiary + organized + constrained → d ≈ 0.55-0.60 → f(d) ≈ 0.75 → χ ≈ 0.68 × 0.75 × 0.9 ≈ 0.46. They experience moderate extraction — they benefit from caste hierarchy relative to lower castes but pay extraction costs to the Brahmin priestly monopoly. No directionality overrides are needed; the structural derivation captures the true relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The high extractiveness (ε=0.68) requires mandatrophy resolution. This constraint resolves mandatrophy by demonstrating that the tension between 'this is coordination' (beneficiary view) and 'this is extraction' (victim view) is not a labeling error but a structural fact: the constraint IS coordination (for Brahmins, it solves the authority problem) AND extraction (for lower castes, it extracts knowledge, status, and resources). The mandatrophy dissolves when we recognize that indexical classification makes both true simultaneously. From the beneficiary's perspective (institutional/arbitrage), the constraint is Rope because it solves a coordination problem: 'Who has authority to perform rituals?' The answer (hereditary Brahmins) feels coordinative and natural to those who benefit from the answer. From the powerless victim's perspective (trapped), the constraint is Snare because the answer locks them into subordinate positions with no exit and extracts their labor/deference. Both classifications are correct — they describe the same institutional mechanism from different structural positions. The mandatrophy is resolved by observing that high-extraction constraints frequently appear as coordination mechanisms to beneficiaries and as pure extraction to victims. This is not a confusion about the constraint's nature; it is the constraint's nature — it coordinates for some by extracting from others. The theater ratio (0.65) further clarifies: actual coordination work (rituals, calendar, community gathering) exists but is mixed with performative hierarchy maintenance. If this were pure extraction with zero coordination, theater would be ~0.85+. If this were pure coordination, theater would be ~0.30. At 0.65, the actual coordination-to-extraction ratio is approximately 35% real function / 65% theatrical assertion. The snare classification from the analytical perspective is appropriate given the suppression level (0.78) and high extractiveness. The temple system does perform coordination, but the asymmetry is too pronounced and the suppression too severe for this to be classified as Rope even from a neutral perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_interpretation_authority,
    'Does hereditary status confer legitimate interpretive authority over Vedic meaning, or is textual authority separable from birth status?',
    'Comparative analysis of hermeneutic traditions: does non-Brahmin scholarly interpretation of Vedic texts produce coherent readings? Can Vedic meaning be established through philology, linguistic analysis, and historical context independent of priestly lineage?',
    'If separable: the hereditary monopoly is contingent extraction, not necessary authority structure. If inseparable: Brahmin hereditary status is semantically constitutive of valid interpretation (rejects the reformist reading''s core premise).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_interpretation_authority, conceptual, 'Whether textual interpretive authority is inseparable from hereditary Brahmin status').

omega_variable(
    ritual_efficacy_empirical_anchor,
    'Does ritual efficacy (cosmic order, blessing, legitimate kingship) depend empirically on Brahmin performance, or is efficacy ascribed through cultural authority regardless of performer status?',
    'Ethnographic and historical analysis: do identical rituals produce different outcomes when performed by Brahmin vs. non-Brahmin actors? Is the claimed difference empirical or performatively constructed? Analysis of ritual outcomes in regions/periods where caste enforcement weakened.',
    'If efficacy is performer-independent: Brahmin monopoly is pure extraction (snare core confirmed). If efficacy requires Brahmin performance: monopoly has coordination function (tangled_rope core supported). Epistemically, this omega reveals whether the constraint''s legitimacy claim rests on verifiable efficacy or authority assertion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_efficacy_empirical_anchor, empirical, 'Whether ritual efficacy empirically depends on Brahmin performer status').

omega_variable(
    knowledge_transmission_capability,
    'Can non-Brahmin populations transmit Vedic knowledge with equivalent fidelity and sophistication, or does hereditary transmission uniquely preserve textual accuracy?',
    'Comparative study of textual accuracy, oral transmission stability, and scholarly innovation: do non-Brahmin scholarly communities (post-colonial universities, reformist movements) produce accurate Vedic scholarship? Historical cases where non-Brahmin learned traditions maintained texts.',
    'If transmission is capability-independent: hereditary monopoly is extractive privilege, not functional necessity. If hereditary transmission uniquely preserves fidelity: the monopoly has genuine coordination function. This omega maps directly to the false summit question: is this natural law or naturalized extraction?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(knowledge_transmission_capability, empirical, 'Whether non-Brahmin transmission of Vedic knowledge is equivalent to hereditary transmission').

omega_variable(
    sibling_reading_foreclosure,
    'Does this hereditary monopoly reading logically foreclose the bhakti devotional reading (direct access to divine without priestly intermediary) or the reformist egalitarian reading (knowledge access independent of birth)?',
    'Hermeneutic analysis: can a single coherent authority framework hold both (a) hereditary priestly monopoly and (b) direct devotional access or egalitarian learning? Historical case study of synthesis attempts in Hindu traditions.',
    'If foreclosure is real: this reading and the siblings are mutually exclusive commitments; only one can be held coherently. If coexistence is possible: readings represent competing institutional interests, not logical contradictions. Defines whether the kernel exhibits genuine foreclosure or mere institutional conflict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether hereditary monopoly logically forecloses rival readings of Vedic authority').

omega_variable(
    female_epistemic_exclusion_necessity,
    'Is female exclusion from Vedic study a necessary implication of hereditary authority structure, or a historically contingent add-on that could theoretically coexist with gender-inclusive transmission?',
    'Textual analysis: does the Vedic corpus itself mandate female exclusion, or is exclusion imposed through institutional interpretation? Historical analysis: are there female Vedic scholars or ritual performers in early or late historical periods? Logical analysis: could a hereditary male-only lineage exist independently of gender-based knowledge restriction?',
    'If necessary: gender exclusion is structurally entailed by this reading''s core commitment. If contingent: gender restriction is a secondary enforcement mechanism that could be removed without collapsing the hereditary structure. Affects the constraint''s clarity and the foreclosure analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(female_epistemic_exclusion_necessity, conceptual, 'Whether female epistemic exclusion is necessary to the hereditary monopoly reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__hereditary_monopoly_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vdc_hm_theater_t0_vedic_period, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(vdc_hm_theater_t5_classical_consolidation, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 5, 0.65).
narrative_ontology:measurement(vdc_hm_theater_t10_medieval_stable, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(vdc_hm_extractiveness_t0_vedic_period, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(vdc_hm_extractiveness_t5_classical_consolidation, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 5, 0.68).
narrative_ontology:measurement(vdc_hm_extractiveness_t10_medieval_stable, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vdc_hm_suppression_t0_vedic_period, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(vdc_hm_suppression_t5_classical_consolidation, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 5, 0.78).
narrative_ontology:measurement(vdc_hm_suppression_t10_medieval_stable, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 10, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__hereditary_monopoly_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus__bhakti_devotional_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus__reformist_egalitarian_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, temple_ritual_economy).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, caste_labor_obligation).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_knowledge_transmission).

% DUAL FORMULATION NOTE:
% The hereditary monopoly reading is one structural reading of how Vedic authority is legitimated. The sibling readings (bhakti devotional, reformist egalitarian) instantiate the same Vedic Corpus kernel with different authority claims and different ε values. Hereditary monopoly reading (ε=0.68, snare) emphasizes extraction and institutional enforcement. Bhakti reading (ε~0.35, rope/tangled_rope) emphasizes direct access without priestly mediation. Reformist reading (ε~0.45, tangled_rope) emphasizes egalitarian learning with institutional resistance. Each reading has its own perspectives, beneficiaries, victims, and measurements. The network links track how each reading influences the others: the hereditary monopoly reading's institutional enforcement creates the resource scarcity that devotional and egalitarian movements resist. The causality runs upstream to downstream: the monopoly reading's institutional structure creates the conditions that demand alternative readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
