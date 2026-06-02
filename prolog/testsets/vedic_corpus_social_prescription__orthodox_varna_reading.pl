% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__orthodox_varna_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_corpus_social_prescription__orthodox_varna_reading, []).

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
 *   constraint_id: vedic_corpus_social_prescription__orthodox_varna_reading
 *   human_readable: Vedic Corpus Social Prescription — Orthodox Varna Reading
 *   domain: religious_studies/social_stratification/hermeneutics
 *
 * SUMMARY:
 *   The orthodox varna reading of the Vedic corpus instantiates a constraint
 *   where the canonical religious texts are interpreted as prescribing a
 *   divinely mandated, permanent, hereditary hierarchy of occupational castes
 *   (varna). This reading claims that the Vedic texts (particularly the
 *   Purusha Sukta creation hymn and ritual manuals) describe the varna system
 *   as cosmic order (rita) — a feature of universal natural law, not a
 *   contingent social arrangement. From this perspective, brahmin priestly
 *   status is mandated by Vedic prescription, kshatriya rule is subordinate
 *   to brahmin ritual authority, vaishya wealth-production is channeled
 *   through brahmin intermediaries, and shudra/dalit populations are excluded
 *   from Vedic study and confined to hereditary occupational roles. The
 *   constraint exhibits high extractiveness (0.68) and extremely high
 *   suppression (0.78) because the system maintains caste boundaries through
 *   ritual prohibition (pollution concepts), occupational monopoly, legal
 *   restrictions on marital and residential freedom, and ideological
 *   naturalization via reincarnation doctrine. The theater ratio (0.55)
 *   reflects that while the constraint involves performative ritual, the
 *   underlying extraction mechanism (occupational assignment, labor
 *   monopsony, status hierarchy) is functional — the constraint is not
 *   primarily maintained through theatrical means, but through material
 *   enforcement and ideological capture.
 *
 * KEY AGENTS:
 *   - Brahmin Priestly Elite: Primary beneficiary (institutional/identity_locked) — monopolizes vedic study, performs sacrifices, receives patronage; identity fused with maintaining varna boundaries
 *   - Kshatriya Warrior-Rulers: Secondary beneficiary (powerful/constrained) — gain authority to rule but constrained to accept brahmin ritual legitimacy; mixed coordination-extraction experience
 *   - Vaishya Merchants/Farmers: Tertiary beneficiary (moderate/mobile) — protected occupational privileges and wealth accumulation within caste boundaries; some exit mobility
 *   - Shudra Occupational Castes: Primary victim (powerless/trapped) — hereditary occupational assignment with no exit; restricted from vedic study, ritual participation; full labor extraction
 *   - Dalit Excluded Communities: Primary victim (powerless/trapped) — outside varna framework entirely; untouchability doctrine; maximum suppression with zero coordination benefit
 *   - Analytical Observer: Civilizational reading (analytical/analytical) — risks naturalizing a contingent institutional extraction as cosmic immutable order
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__orthodox_varna_reading, 0.68).
domain_priors:suppression_score(vedic_corpus_social_prescription__orthodox_varna_reading, 0.78).
domain_priors:theater_ratio(vedic_corpus_social_prescription__orthodox_varna_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__orthodox_varna_reading, snare).
narrative_ontology:human_readable(vedic_corpus_social_prescription__orthodox_varna_reading, "Vedic Corpus Social Prescription — Orthodox Varna Reading").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__orthodox_varna_reading, "religious_studies/social_stratification/hermeneutics").

domain_priors:requires_active_enforcement(vedic_corpus_social_prescription__orthodox_varna_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__orthodox_varna_reading, 'e80d7ad2-701e-4737-8763-a6c46c161262').
narrative_ontology:cs_kernel_codification('e80d7ad2-701e-4737-8763-a6c46c161262', fixed_text).
narrative_ontology:cs_authority_grounding('e80d7ad2-701e-4737-8763-a6c46c161262', lineage).
narrative_ontology:cs_interpretation_layer_present('e80d7ad2-701e-4737-8763-a6c46c161262').
narrative_ontology:cs_reading_relation('e80d7ad2-701e-4737-8763-a6c46c161262', vedic_corpus_social_prescription__reformist_spiritual_reading, coexists_with).
narrative_ontology:cs_reading_relation('e80d7ad2-701e-4737-8763-a6c46c161262', vedic_corpus_social_prescription__colonial_orientalist_reading, coexists_with).
narrative_ontology:cs_axiom('e80d7ad2-701e-4737-8763-a6c46c161262', foundational, varna_permanently_prescriptive).
narrative_ontology:cs_axiom_status(varna_permanently_prescriptive, holdable).
narrative_ontology:cs_axiom_grounding('e80d7ad2-701e-4737-8763-a6c46c161262', varna_permanently_prescriptive, deontological).
narrative_ontology:cs_axiom('e80d7ad2-701e-4737-8763-a6c46c161262', foundational, brahmin_ritual_authority_necessity).
narrative_ontology:cs_axiom_status(brahmin_ritual_authority_necessity, holdable).
narrative_ontology:cs_axiom_grounding('e80d7ad2-701e-4737-8763-a6c46c161262', brahmin_ritual_authority_necessity, deontological).
narrative_ontology:cs_reference_frame('e80d7ad2-701e-4737-8763-a6c46c161262', vedic_cosmic_order_foundation).
narrative_ontology:cs_drift_state('e80d7ad2-701e-4737-8763-a6c46c161262', contemporary_post_colonial_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e80d7ad2-701e-4737-8763-a6c46c161262', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_priestly_elite).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, shudra_occupational_castes).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, dalit_excluded_communities).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, women_across_castes).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SHUDRA OCCUPATIONAL CASTES (SNARE) — Vedic prescription mandates hereditary occupational assignment, marital endogamy, and exclusion from Vedic study. Exit from birth-caste occupation is structurally prohibited — reincarnation doctrine naturalizes permanent assignment. Suppression is enforced through ritual prohibition (pollution concepts), occupational monopoly by higher castes, and denial of educational access. Maximum experienced extraction: restricted labor value extraction with zero structural exit option.
constraint_indexing:constraint_classification(vedic_corpus_social_prescription__orthodox_varna_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: DALIT EXCLUDED COMMUNITIES (SNARE) — Outside the varna framework entirely. Vedic text prescribes untouchability: proximity pollution, occupation restrictions (disposal of carrion, human waste), habitat segregation, and denial of religious participation. Suppression through ritual defilement concepts and legal exclusion. Pure extraction with no coordination function — the constraint exists solely to maintain caste boundary and extract labor without reciprocal status or resources.
constraint_indexing:constraint_classification(vedic_corpus_social_prescription__orthodox_varna_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: BRAHMIN PRIESTLY ELITE (SNARE) — Vedic prescription centralizes ritual authority (vedic study monopoly, sacrifice performance, interpretation), occupational privilege (state patronage, tax exemption, land grants), and social status (purity rules that privilege brahmin conduct). Beneficiary from extraction while being captured by the constraint's identity requirements — brahmin identity becomes inseparable from maintaining varna boundaries. High extraction benefit, high identity lock preventing recognition of system's contingency.
constraint_indexing:constraint_classification(vedic_corpus_social_prescription__orthodox_varna_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 4: KSHATRIYA WARRIOR-RULER CASTE (TANGLED ROPE) — Vedic prescription grants authority to rule and access to secular power, but subordinates political authority to brahmin ritual authority (brahmin priest as legitimizer). Genuine coordination function: the varna frame redistributes resources (tax revenue to brahmins for ritual maintenance) in exchange for legitimacy conferral. Also asymmetric extraction: kshatriyas are constrained to accept brahmin ritual prerogatives. Moderate experienced extraction — significant agency but embedded in a system of constraints.
constraint_indexing:constraint_classification(vedic_corpus_social_prescription__orthodox_varna_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: VAISHYA MERCHANT-FARMER CASTE (ROPE) — Vedic prescription grants occupational privilege and wealth accumulation within defined boundaries. Lower extraction than higher castes but benefits from the system: protected markets (monopoly on trade/agriculture by caste), stable labor supply (shudra dependence), and social status above shudras. Some exit mobility (occupational variation within vaishya roles, ritual compensation mechanisms). Coordination function dominates: the varna framework stabilizes economic roles and supply chains.
constraint_indexing:constraint_classification(vedic_corpus_social_prescription__orthodox_varna_reading, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW READING (MOUNTAIN) — From a civilizational analytical position, the varna scheme is presented as an immutable cosmic order (rita — universal law), not a contingent social construction. Each varna occupies its proper position in the universal hierarchy by nature and dharma. No agent exits because the system reflects cosmic necessity. However, this classification is a FALSE SUMMIT: the constraint has identifiable beneficiaries (brahmin priestly elite) and structured extraction mechanisms (labor monopsony, status extraction, ritual prohibition). The naturalization is deliberate — the doctrine works precisely by claiming cosmic immutability.
constraint_indexing:constraint_classification(vedic_corpus_social_prescription__orthodox_varna_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_corpus_social_prescription__orthodox_varna_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(vedic_corpus_social_prescription__orthodox_varna_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(vedic_corpus_social_prescription__orthodox_varna_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(vedic_corpus_social_prescription__orthodox_varna_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vedic_corpus_social_prescription__orthodox_varna_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High-snare range. The constraint extracts occupational labor value from shudra and dalit populations through hereditary assignment with zero exit options, enforces status hierarchy that restricts marriage and ritual participation, and concentrates ritual authority and its material rewards (patronage, land grants, tax exemption) in the brahmin elite. The extraction is not total (0.90+) because some vaishya wealth-creation occurs within the system, and some kshatriya political authority is genuine — but the skew toward extraction is severe. The rising trajectory (0.55 → 0.68) over the measurement interval reflects historical enforcement intensification: as brahmin institutional power consolidated and reincarnation doctrine became integrated into philosophical systems, the naturalization of varna boundaries deepened, reducing subjective perception of contingency and increasing structural suppression required to maintain boundaries. Suppression (0.78): Very high. Enforcement through ritual prohibition (pollution/untouchability), occupational monopoly by higher castes, legal restrictions on land ownership and residence, denial of educational access, and ideological capture via reincarnation doctrine (hereditary assignment is justified as karmic consequence). The rising trajectory (0.70 → 0.78) reflects intensification of enforcement mechanisms as resistance grew. Theater ratio (0.55): Moderate. The constraint includes performative ritual (Vedic sacrifice, purity rituals), but the primary mechanism is structural (occupational assignment, resource monopoly, legal restriction). The constraint functions through material enforcement as much as through theatrical performance.
 *
 * PERSPECTIVAL GAP:
 *   The orthodox varna reading produces maximal perspectival divergence across the status hierarchy. Powerless trapped agents (shudra/dalit) experience snare with maximum chi. Beneficiaries with identity lock (brahmin elite) also classify as snare but rationalize it as dharmic duty — the identity lock prevents them from perceiving the extraction as extraction. Constrained beneficiaries (kshatriya) see tangled rope — genuine coordination alongside constraint. Mobile beneficiaries (vaishya) see rope with stable constraints. The analytical observer risks mountain (cosmic order) but structural data reveals false summit. The perspectival gap reveals how the constraint naturalizes itself: different observers experiencing different classifications of the same phenomenon, with beneficiaries experiencing snare-but-calling-it-coordination while victims experience snare-as-snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d value) and resulting effective extractiveness (chi) derive from the agent's power level, exit options, and structural relationship to the extraction flow. Brahmin priestly elite: beneficiary status + identity_locked exit → d ≈ 0.20 (moderate beneficiary capture, high identity lock preventing exit perception). The beneficiary with identity lock is captured: they benefit but cannot exit psychologically. Kshatriya rulers: mixed beneficiary/constrained → d ≈ 0.48 (both benefits and constraints, moderate status). Vaishya merchants: beneficiary + mobile → d ≈ 0.35 (can exit at moderate cost, benefits within system). Shudra/dalit: victim + trapped → d ≈ 0.92 (maximum target, no exit). The directionality divergence is diagnostic: the same constraint produces radically different d values and chi across the status hierarchy, which is exactly what makes it a snare — the distribution of power means some agents experience benign coordination while others experience coercive extraction. No override needed here; the structural derivation produces the correct d values.
 *
 * MANDATROPHY ANALYSIS:
 *   READING-SPECIFIC MANDATROPHY: The orthodox varna reading is a reading, not a fact about the Vedic corpus. The mandatrophy is resolved by the committer frame (Rules 1–4): this reading claims the Vedic corpus prescribes permanent hereditary varna as cosmic order, instantiating a snare. The reformist reading claims the Vedic corpus teaches spiritual equality and rejects social prescription. The orientalist reading claims the corpus is a colonial artifact reified through orientalist interpretation. Each reading instantiates different constraints with different ε values and different beneficiary/victim sets. The classification snare is stable within the orthodox reading — the mandate (cosmic prescriptiveness of varna) is clear, the extraction is high, the suppression is severe. The mandatrophy is not 'is varna a snare?' but 'which reading of the Vedic corpus are you instantiating?' This story answers: the orthodox reading instantiates a snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    orthopraxy_versus_belief_binding,
    'Is the varna constraint primarily enforced through orthopraxy (correct ritual action and caste practice) or through genuine ideological belief in cosmic order?',
    'Historical analysis of dissent: if challenging cosmic order occurs but orthopraxy persists (due to material penalties), binding is primarily external. If belief in cosmic order sustains orthopraxy, binding is ideological. Cross-cultural comparison with non-Vedic caste-like systems that lack cosmic legitimation.',
    'If orthopraxy-dominant: the constraint is a snare maintained by material suppression; exit is structurally possible if penalties are removed. If belief-dominant: the constraint includes identity-lock mechanisms that persist beyond material suppression removal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(orthopraxy_versus_belief_binding, empirical, 'Whether varna constraint is enforced through orthopraxy or ideological belief').

omega_variable(
    reincarnation_doctrine_causal_role,
    'Does the reincarnation doctrine (dvija — twice-born, karmic rebirth into caste) functionally enable the varna constraint''s extraction, or is it primarily theological window dressing?',
    'Historical records of resistance movements: do anti-caste movements explicitly target and reject reincarnation doctrine? Do movements that accept reincarnation but reject varna hierarchy exist? Comparative analysis with non-reincarnation-based caste-like systems.',
    'If causal: reincarnation naturalizes hereditary assignment and eliminates exit justification; removing it should materially weaken the constraint. If window dressing: the constraint persists on material enforcement alone and reincarnation rejection doesn''t change extraction dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reincarnation_doctrine_causal_role, empirical, 'Whether reincarnation doctrine is causal to varna constraint enforcement').

omega_variable(
    ritual_monopoly_versus_ideological_hegemony,
    'Is the brahmin monopoly on vedic knowledge and ritual performance primarily a material resource monopoly or an ideological hegemony?',
    'Historical data on brahmin economic dependence on ritual patronage vs. independent land/trade wealth. Evidence of alternative ritual specialists (non-brahmin priests) and whether they were marginalized. Post-colonial era data on whether brahmin economic status persists without ritual monopoly.',
    'If monopoly is primarily material: brahmins can be displaced through economic competition without doctrinal change. If hegemony is primarily ideological: brahmins retain status even after material privilege erodes, suggesting identity-lock on both sides.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ritual_monopoly_versus_ideological_hegemony, empirical, 'Whether brahmin privilege rests on ritual monopoly or ideological hegemony').

omega_variable(
    kernel_versus_reading_ambiguity,
    'Can the Vedic corpus be read as anything other than a varna-prescriptive text, or does this reading impose modern categories on pre-categorical sources?',
    'Textual analysis: do foundational Vedic passages (Purusha Sukta, Manusmriti precursor texts) unambiguously prescribe varna as cosmic/permanent order? Or do they describe varna descriptively/contextually? Cross-reading analysis with reformist and orientalist readings to map which textual passages each reading emphasizes.',
    'If reading is imposed: the orthodox reading is one contestable hermeneutic choice, not an inevitable extraction of textual meaning. If reading is textually grounded: the constraint is genuinely embedded in the canonical source. Classification remains snare either way, but omega distinguishes authorial intent from interpretive claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_versus_reading_ambiguity, conceptual, 'Whether varna prescription is textually necessary or hermeneutically imposed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__orthodox_varna_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedic_varna_tr_t0, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(vedic_varna_tr_t5, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 5, 0.52).
narrative_ontology:measurement(vedic_varna_tr_t10, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(vedic_varna_be_t0, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(vedic_varna_be_t5, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(vedic_varna_be_t10, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vedic_varna_su_t0, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(vedic_varna_su_t5, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 5, 0.74).
narrative_ontology:measurement(vedic_varna_su_t10, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 10, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__orthodox_varna_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription__reformist_spiritual_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription__colonial_orientalist_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, dalit_exclusion_pollution_doctrine).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_vedic_monopoly).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, occupational_heredity_enforcement).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel. The sibling readings (reformist, orientalist) instantiate structurally different constraints with different ε values from the same source text. The upstream constraints (dalit_exclusion_pollution_doctrine, brahmin_vedic_monopoly, occupational_heredity_enforcement) are structural mechanisms that the orthodox varna reading coordinates into a single system. Decomposition: if you measure the constraint as 'the Vedic texts prescribe varna' vs 'brahmin institutions enforce varna boundaries,' you have two different ε values and two different constraint stories. This story models the textual prescription reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
