% ============================================================================
% CONSTRAINT STORY: colonial_caste_categorization_artifact
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_colonial_caste_categorization_artifact, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: colonial_caste_categorization_artifact
 *   human_readable: British Colonial Census Formalization of Caste
 *   domain: religion/colonial_imposition/administrative_categorization
 *
 * SUMMARY:
 *   The British colonial census, formally implemented from 1871 onward,
 *   transformed the operational reality of caste in South Asia by imposing
 *   fixed, singular categorical assignments on social identities that had
 *   operated through multiple overlapping affiliations, ritual contexts, and
 *   regional variations. Prior to colonial enumeration, caste (jati) systems
 *   existed but functioned through fluid, negotiable, and locally-specific
 *   mechanisms — a person's status was context-dependent, ritual-defined, and
 *   subject to regional variation and historical change. The census apparatus
 *   required every subject to declare a single, permanent caste category for
 *   administrative purposes, creating bureaucratic identity that became
 *   legally binding, heritable, and the basis for subsequent policy
 *   discrimination and resource allocation. This constraint exhibits all six
 *   classification types from different structural positions, making it a
 *   diagnostic exemplar for colonial epistemic violence and the persistence
 *   of categorical legacies post-independence. The critical methodological
 *   caveat: characterizations of both pre-colonial caste and the colonial
 *   census effects depend on what evidence we treat as reliable. Colonial-era
 *   records reflect colonial categorization logic; pre-colonial sources are
 *   fragmentary and linguistically diverse. The constraint we study may
 *   partially be a product of colonial description itself — we may be
 *   observing the constraint's impact on how caste is understood rather than
 *   separating pre-colonial reality from colonial imposition. This epistemic
 *   circularity strengthens the case for false-summit detection: the mountain
 *   perspective risks treating colonial categorization as natural law when it
 *   is historically contingent colonial design.
 *
 * KEY AGENTS:
 *   - Fluid Identity Communities: Primary victim (powerless/trapped) — pre-colonial communities whose identities operated across multiple ritual roles and regional contexts. Trapped by permanent bureaucratic categorization. Experience maximum extraction with no exit mechanism.
 *   - Intermediate Status Groups: Primary victim (powerless/trapped) — pre-colonial groups whose status was context-dependent or ritually-specific. Forced into fixed hierarchical positions they did not occupy. Experience maximum epistemic violence.
 *   - Untouchable Castes: Primary victim (powerless/trapped) — pre-colonial groups with internal diversity in function and regional autonomy. Colonial census transforms untouchability into fixed administrative category enabling subsequent legal exclusion and labor coercion.
 *   - Colonial Administrative Apparatus: Primary beneficiary (institutional/arbitrage) — solves legitimate operational need for systematic categorization while simultaneously enabling control, taxation, recruitment, and enforcement of colonial hierarchies. Experiences constraint as tangled rope: genuine coordination function inseparable from extraction.
 *   - Brahminical Elite and Collaboration Networks: Secondary beneficiary (organized/constrained) — benefit from census formalization that locks traditional hierarchies into administrative law. Embedded in colonial relationships; cannot fully exit without loss of influence. Constrained rather than trapped.
 *   - Reform and Anti-Caste Movements: Organized resistance (organized/mobile) — perceive census categorization as enabling political organization by making caste boundaries administratively visible. Mobile exit via cultural reform, political representation, constitutional change. Low theater because they see through bureaucratic naturalization.
 *   - Post-Colonial Constitutional and Legal Reform: Organized institutional reform (organized/constrained) — use colonial categories for constitutional reserved representation, creating paradox where colonial categories enable emancipatory policy. Constrained by need to maintain categorical systems while dismantling categorical discrimination.
 *   - Categorical Bureaucratic Inertia: Institutional persistence (institutional/constrained) — colonial categories persist in post-colonial administration through momentum across dozens of systems. Theater high; categories are maintained as administrative necessity while their colonial origins are obscured.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing colonial categorization as inherent bureaucratic necessity. False summit candidate — treating 'some categorization is inevitable' as equivalent to 'this specific colonial categorization is necessary'.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(colonial_caste_categorization_artifact, 0.58).
domain_priors:suppression_score(colonial_caste_categorization_artifact, 0.72).
domain_priors:theater_ratio(colonial_caste_categorization_artifact, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(colonial_caste_categorization_artifact, extractiveness, 0.58).
narrative_ontology:constraint_metric(colonial_caste_categorization_artifact, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(colonial_caste_categorization_artifact, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(colonial_caste_categorization_artifact, tangled_rope).
narrative_ontology:human_readable(colonial_caste_categorization_artifact, "British Colonial Census Formalization of Caste").
narrative_ontology:topic_domain(colonial_caste_categorization_artifact, "religion/colonial_imposition/administrative_categorization").

domain_priors:requires_active_enforcement(colonial_caste_categorization_artifact).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(colonial_caste_categorization_artifact, colonial_administrative_apparatus).
narrative_ontology:constraint_beneficiary(colonial_caste_categorization_artifact, brahminical_elite_groups).
narrative_ontology:constraint_victim(colonial_caste_categorization_artifact, fluid_identity_communities).
narrative_ontology:constraint_victim(colonial_caste_categorization_artifact, intermediate_status_groups).
narrative_ontology:constraint_victim(colonial_caste_categorization_artifact, untouchable_castes).
narrative_ontology:constraint_victim(colonial_caste_categorization_artifact, epistemic_commons_of_caste_understanding).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FLUID IDENTITY COMMUNITIES (SNARE) — Powerless agents whose pre-colonial identities operated across multiple overlapping affiliations, ritual statuses, and regional variations. Trapped by census categorization that forces singular, permanent caste assignment. No appeal mechanism, no exit from bureaucratic identity once recorded. The extraction is maximal: the constraint rewrites their social ontology without consent and makes the rewritten version the official reality for property, employment, and legal status. High suppression because colonial force backs the categorization and pre-colonial fluidity is no longer an available option post-enumeration.
constraint_indexing:constraint_classification(colonial_caste_categorization_artifact, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INTERMEDIATE STATUS GROUPS (SNARE) — Groups whose pre-colonial status was context-dependent, ritual-specific, or regionally variable experience maximum extraction from binary hierarchical categorization. The census forces them into fixed positions on a scale they did not inhabit. Trapped: colonial law enshrines the census category as official identity. Suppression is high because the categorization is backed by administrative power and replaces pre-colonial plurality with singular legal identity. These groups face the most severe epistemic violence — their actual social complexity is rendered invisible and replaced with a colonial simplification.
constraint_indexing:constraint_classification(colonial_caste_categorization_artifact, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: UNTOUCHABLE CASTES (SNARE) — Pre-colonial untouchability operated through ritual prohibition and occupational restriction but maintained significant internal diversity in economic function, regional status, and community autonomy. The colonial census transforms untouchability into a fixed administrative category, which then becomes the basis for systematic legal exclusion, landlessness, and labor coercion. Trapped: the census classification locks them into the lowest administrative tier, enabling subsequent legislation and policy discrimination. Suppression is maximal because colonial enforcement power prevents any renegotiation of categorical boundaries. The constraint extracts labor, land, and dignity without coordination benefit.
constraint_indexing:constraint_classification(colonial_caste_categorization_artifact, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: COLONIAL ADMINISTRATIVE APPARATUS (TANGLED ROPE) — The census machinery solves a genuine coordination problem: how to administer a diverse population across linguistic and cultural boundaries. The British face a legitimate operational need for systematic categorization. However, the coordination function is inseparable from extraction: the categorization also enables control, taxation, recruitment for military and labor forces, and enforcement of colonial hierarchies. The apparatus benefits from the constraint (arbitrage exit — it can adjust procedures if needed), experiences it as coordination (solves administrative need), but simultaneously extracts authority, revenue, and labor from the population. Requires active enforcement: colonial military and bureaucratic apparatus backs the categorization. High suppression because the apparatus can override any local or individual dissent.
constraint_indexing:constraint_classification(colonial_caste_categorization_artifact, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: BRAHMINICAL ELITE AND COLLABORATION NETWORKS (TANGLED ROPE) — Pre-colonial brahminical elites benefit from census formalization that locks traditional hierarchies into administrative law. The constraint coordinates brahminical influence (legitimate: providing cultural authority for categorization) while enabling extraction (preferential treatment, collaboration leverage, protection from below). Constrained exit: these groups are embedded in colonial administrative relationships and cannot fully exit without loss of status and influence. Active enforcement required: collaboration with census apparatus necessary to maintain extracted benefits. Suppression moderate-high: backed by colonial force but negotiated through elite collaboration rather than direct coercion of elites themselves.
constraint_indexing:constraint_classification(colonial_caste_categorization_artifact, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: REFORM AND ANTI-CASTE MOVEMENTS (ROPE) — Indigenous reform movements (Arya Samaj, Brahmo Samaj, Ambedkar's mobilization) perceive the colonial census categorization as a coordination mechanism that makes visible the hierarchies they oppose. The constraint enables political organization by making caste boundaries administratively salient and legally challengeable. Mobile exit: reform groups can shift strategy between cultural reform, political representation, and constitutional change. No maximum extraction experienced because the constraint is perceived as providing the very legibility required for resistance. Theater low: the reform movements see through the bureaucratic naturalization of colonial categories.
constraint_indexing:constraint_classification(colonial_caste_categorization_artifact, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: POST-COLONIAL CONSTITUTIONAL AND LEGAL REFORM (SCAFFOLD) — The Indian Constitution explicitly rejects caste-based discrimination (Article 17) and establishes reserved seats based on census categories, creating a paradox: using colonial categories to dismantle colonial extraction. This perspective sees the census categorization as a temporary institutional form with a sunset clause embedded in the constitutional order. Constrained exit: legal reforms cannot simply abandon the categories (they are now embedded in reserved representation systems) but can theoretically dismantle caste-based discrimination once scheduled caste/tribe status is no longer needed. The scaffold assumes that categorical legacies will fade as substantive equality is achieved, though the assumption remains contested. Theater moderate: the legal system performs equality while categorical legacies persist.
constraint_indexing:constraint_classification(colonial_caste_categorization_artifact, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: CATEGORICAL BUREAUCRATIC INERTIA (PITON) — The colonial census categories persist in post-colonial India through administrative inertia despite their contested legitimacy. They structure employment records, educational reservations, marriage registration, and census re-enumeration. The categories are maintained not because they reflect social reality (the ground has shifted significantly since 1871) but because replacing them would require simultaneous reform across dozens of administrative systems. Theater high: the bureaucratic categorization performs as natural administrative necessity while its colonial origins are obscured. Constrained exit: institutions are locked into using categories because alternatives are uncoordinated. The piton perspective observes that the categorical apparatus is degraded — it does not match contemporary caste dynamics, which are more fluid, politicized, and context-dependent than the 1871 categories captured — but persists through institutional momentum.
constraint_indexing:constraint_classification(colonial_caste_categorization_artifact, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / ESSENTIALISM RISK (MOUNTAIN) — From a civilizational/universal perspective, some categorization of social statuses is inherent to any large-scale society. Fixed categories might appear as an inevitable structural feature of bureaucratic governance itself. However, this perspective risks naturalizing what is historically contingent: the colonial choice to formalize and fix what had been fluid and renegotiable. The mountain classification is a false summit candidate — it treats colonial categorization as a natural law of social organization rather than a specific historical imposition. The analytical observer's risk is treating 'categorization exists' (true) as equivalent to 'this specific categorization is necessary and inevitable' (false).
constraint_indexing:constraint_classification(colonial_caste_categorization_artifact, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(colonial_caste_categorization_artifact_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(colonial_caste_categorization_artifact, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(colonial_caste_categorization_artifact, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(colonial_caste_categorization_artifact, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(colonial_caste_categorization_artifact, TR),
    TR >= 0.70.

:- end_tests(colonial_caste_categorization_artifact_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts in multiple dimensions: (1) epistemic extraction — pre-colonial plurality is replaced with colonial simplification; (2) legal extraction — categorical assignment becomes basis for discrimination and resource denial; (3) administrative extraction — categorization enables taxation, recruitment, and labor control; (4) identity extraction — subjects' self-understanding is overwritten by bureaucratic assignment. The value is moderate rather than very high (0.70+) because some groups experience coordination benefits through census-enabled mobilization and post-colonial reserved representation, which partially offsets extraction. Suppression (0.72): High. Backed by colonial military force, colonial law, and colonial bureaucratic authority, the categorization cannot be appealed or renegotiated at individual level. Post-colonial suppression mechanisms are more subtle (administrative inertia, categorical embedding in multiple systems) but still substantial. Theater ratio (0.65): Moderate-high and rising. The colonial census presented categorization as scientific enumeration and rational administration when it was actually a categorization choice that naturalized and fixed what had been fluid. Post-colonial theater increased (rising to 0.72 by 1951) as the categorical system became institutionalized and its contingency was obscured. Contemporary theater (0.65) is declining as consciousness of colonial origins has risen and post-colonial reform has made the categorical basis more explicit, though administrative inertia still performs theater.
 *
 * PERSPECTIVAL GAP:
 *   The fundamental perspectival gap is between agents experiencing the constraint as extraction (powerless/trapped) versus agents experiencing it as coordination with extraction (institutional/arbitrage). A secondary gap separates colonial-era experience (extractiveness rising from 0.42 to 0.62 as categorization became entrenched) from post-colonial experience (extractiveness stabilizing or declining as formal discrimination was prohibited, though categorical legacies persist). The tertiary gap is epistemic: whether the constraint represents imposition of fixity on pre-colonial fluidity (strong violence narrative) or formalization of already-existing hierarchies (weaker violence narrative). The amplitude of perspectival divergence indicates that the constraint resolves mandatrophy through multiplicity: there is no single 'correct' classification — the presheaf of perspectives over the observation site is the answer.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness (χ) is computed from base extractiveness (ε=0.58), directionality-derived f(d), and scope modifier σ(S=1.0, national). Powerless trapped agents experience maximum χ: 0.58 × 1.40 × 1.0 ≈ 0.81. Colonial apparatus experiences minimal χ: 0.58 × 0.05 × 1.0 ≈ 0.03 (negative effective extraction — the constraint subsidizes their governance). Intermediate agents experience moderate χ: 0.58 × 0.40-0.75 × 1.0 ≈ 0.23-0.44. The directionality variation drives perspectival classification differences: snare emerges from high d + high χ; rope emerges from low d and coordination function; tangled rope emerges from moderate d with both coordination and extraction visible. No overrides are needed — the structural derivation chain captures the primary directionality distinctions.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in this constraint is: 'Is this a natural feature of bureaucratic governance (mountain) or a historical imposition enabled by colonial power (snare/tangled_rope/scaffold)?'. The resolution path depends on three factors: (1) the true fluidity of pre-colonial caste systems (omega_1), (2) the degree to which categorical legacies reflect colonial design vs pre-existing hierarchy (omega_2), and (3) whether post-colonial reform has actually dissolved the categorical constraint or merely inverted it (omega_4). The constraint resolves mandatrophy through perspectival multiplicity rather than reclassification. Each perspective captures a genuine structural truth: the administrative apparatus does solve a coordination problem (rope/tangled_rope true from their view); powerless agents do experience pure extraction without coordination benefit (snare true from their view); post-colonial reform does theoretically enable category dissolution (scaffold true as long-term trajectory); the categories do persist through institutional inertia despite their functional degradation (piton true for contemporary bureaucracy); and the analytical observer does risk naturalizing what is contingent (false summit mountain, true). The FALSE SUMMIT DETECTION signature on the mountain perspective is active because: (1) the constraint declares multiple beneficiaries (colonial apparatus, brahminical elites); (2) the beneficiary presence contradicts natural law classification; (3) the engine will compute this as a false summit and recommend reclassification. The mountain perspective serves as a diagnostic for how naturalizing narratives ('categorization is inherent to bureaucracy') can occlude historical contingency ('this specific categorization was a colonial choice'). No single classification is 'correct' — the constraint is genuinely a tangled_rope from institutional perspectives (coordination + extraction), a snare from powerless perspectives (pure extraction), a scaffold from reform perspectives (theoretically temporary), and a piton from bureaucratic inertia perspectives (degraded function, maintained through momentum). The false summit mountain perspective reveals the framework's utility: forcing us to examine whether 'natural law of governance' is an accurate description or a naturalization of colonial design.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pre_colonial_caste_fluidity_degree,
    'How fluid and plural were pre-colonial caste identities across different regions and time periods? Was the colonial census imposing fixity on something genuinely fluid, or merely formalizing existing hierarchies?',
    'Historical linguistic and textual analysis of pre-colonial jati references, regional variation studies, comparison of colonial-era recorded identities with oral histories collected post-independence, archaeological evidence of occupational mobility patterns',
    'If pre-colonial system was highly fluid: colonial constraint represents major epistemic violence and fixity imposition (high extraction). If pre-colonial system was already substantially hierarchical: constraint formalizes rather than invents hierarchy (moderate extraction, less novel violence). The extractiveness value (0.58) assumes moderate pre-colonial fluidity — significant but not total. If evidence shifts toward high fluidity, extractiveness should rise to 0.70+; if toward already-fixed hierarchy, should drop to 0.35.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pre_colonial_caste_fluidity_degree, empirical, 'Degree of fluidity in pre-colonial jati system').

omega_variable(
    colonial_elite_collaboration_causation,
    'Did brahminical elites actively collaborate with colonial census apparatus, or were they incidental beneficiaries of an apparatus they did not design?',
    'Archival analysis of colonial census correspondence, elite petitions, examination of which groups successfully lobbied for specific categorizations, comparison of census outcomes with pre-existing brahminical classification texts (Manusmriti derivatives, local hierarchies)',
    'If active collaboration: brahminical elites are primary beneficiaries (strong tangled_rope from institutional perspective). If passive beneficiaries: the constraint is more purely extractive colonial imposition (snare becomes dominant). Active collaboration strengthens the institutional perspective''s tangled_rope classification and justifies the beneficiary declaration; passive beneficiaries weaken coordination function claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_elite_collaboration_causation, empirical, 'Extent of brahminical elite collaboration in census categorization').

omega_variable(
    census_categorical_stability_post_colonial,
    'Have census categories remained stable, diverged from ground social reality, or been substantially reconstructed across post-colonial censuses (1951, 1961, 1971, etc.)?',
    'Comparison of categorical definitions and individual classifications across decennial censuses, analysis of re-enumeration results for same geographic areas, tracking of groups that shifted categories between census cycles, documentation of administrative amendments to category definitions',
    'If categories remain essentially identical to 1871: the categorical legacies are massive and persistent (piton theater ratio should rise above 0.70). If categories have shifted substantially: post-colonial reconstruction is reducing the colonial artifact status (scaffold perspective gains credibility). Current analysis assumes moderate stability with adaptation — if evidence shows high stability, piton dominates; if evidence shows high fluidity, scaffold confidence increases.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(census_categorical_stability_post_colonial, empirical, 'Stability and adaptation of census categories across post-colonial censuses').

omega_variable(
    reserved_seats_paradox_resolution,
    'Does the use of colonial census categories for constitutional reserved representation perpetuate colonial categorization, or does it strategically invert it for emancipatory purposes?',
    'Analysis of whether reserved representation has led to substantive equalization and category dissolution, or entrenched categories as permanent political and administrative identities; comparison of mobility rates and economic outcomes for scheduled caste/tribe vs non-reserved groups across decades; examination of whether political representation itself demands continued categorical maintenance',
    'If paradox enables emancipation: scaffold perspective is empirically validated and extraction is genuinely temporary (sunset clause functional). If paradox perpetuates or deepens categorical extraction: the constraint persists as tangled_rope or snare despite formal legal prohibition. The mandatrophy remains unresolved pending this determination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reserved_seats_paradox_resolution, empirical, 'Whether reserved representation enables category dissolution or perpetuates categorical legacies').

omega_variable(
    identity_lock_vs_structural_constraint,
    'For post-colonial subjects whose identities were formed within colonial categorical systems, is caste affiliation identity_locked (internalized, identity-constitutive) or merely structurally constrained (external administrative barriers)?',
    'Ethnographic and interview-based study of how caste identity is articulated across generations, comparison of internal identity conception (how people describe themselves) vs external administrative assignment, analysis of post-mobility identity shifts (do people retain caste affiliation after upward mobility or geographic displacement?)',
    'If strongly identity_locked: the constraint persists through internalization even as structural barriers decline (high suppression, persistent extraction). If primarily constrained: removal of administrative barriers could enable fluidity (scaffold perspective gains credibility). Understanding this distinction is critical for predicting whether post-colonial reform can actually dissolve the categorical constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_constraint, empirical, 'Whether post-colonial caste affiliation is identity-locked or structurally constrained').

omega_variable(
    measurement_epistemic_circularity,
    'Does the constraint create an epistemic trap where the categorization system is both the measurement apparatus and the phenomenon being measured? That is, are we studying ''real'' pre-colonial caste or studying colonial categories?',
    'Methodological assessment of source materials — are descriptions of pre-colonial caste derived from colonial-era records (which inherit census logic) or from genuinely pre-colonial texts and oral histories? Can we disentangle colonial categorization from pre-colonial reality, or is the constraint so complete that the two are now inseparable?',
    'If high circularity: the constraint cannot be fully characterized because our evidence about pre-colonial reality is contaminated by colonial categorization. The false summit mountain classification becomes stronger — we may be unable to escape the colonial frame to see what was imposed. This meta-level uncertainty strengthens the case for FSM evaluation (false summit detection on the mountain perspective).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(measurement_epistemic_circularity, conceptual, 'Epistemic circularity of colonial categorical measurement systems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(colonial_caste_categorization_artifact, 1871, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_1871_initial, colonial_caste_categorization_artifact, theater_ratio, 1871, 0.5).
narrative_ontology:measurement(theater_1891_midpoint, colonial_caste_categorization_artifact, theater_ratio, 1891, 0.58).
narrative_ontology:measurement(theater_1921_entrenchment, colonial_caste_categorization_artifact, theater_ratio, 1921, 0.68).
narrative_ontology:measurement(theater_1951_post_independence, colonial_caste_categorization_artifact, theater_ratio, 1951, 0.72).
narrative_ontology:measurement(theater_2000_late_reform, colonial_caste_categorization_artifact, theater_ratio, 2000, 0.68).
narrative_ontology:measurement(theater_2024_contemporary, colonial_caste_categorization_artifact, theater_ratio, 2024, 0.65).

% Extraction over time
narrative_ontology:measurement(extractiveness_1871_initial, colonial_caste_categorization_artifact, base_extractiveness, 1871, 0.42).
narrative_ontology:measurement(extractiveness_1891_midpoint, colonial_caste_categorization_artifact, base_extractiveness, 1891, 0.55).
narrative_ontology:measurement(extractiveness_1921_entrenchment, colonial_caste_categorization_artifact, base_extractiveness, 1921, 0.62).
narrative_ontology:measurement(extractiveness_1951_post_independence, colonial_caste_categorization_artifact, base_extractiveness, 1951, 0.58).
narrative_ontology:measurement(extractiveness_2000_late_reform, colonial_caste_categorization_artifact, base_extractiveness, 2000, 0.56).
narrative_ontology:measurement(extractiveness_2024_contemporary, colonial_caste_categorization_artifact, base_extractiveness, 2024, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(colonial_caste_categorization_artifact, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(colonial_caste_categorization_artifact, 0.12).
narrative_ontology:affects_constraint(colonial_caste_categorization_artifact, reserved_seat_paradox_post_colonial).
narrative_ontology:affects_constraint(colonial_caste_categorization_artifact, brahminical_ritual_authority_legitimacy).
narrative_ontology:affects_constraint(colonial_caste_categorization_artifact, scheduling_administrative_identity).

% DUAL FORMULATION NOTE:
% The colonial census categorization is the upstream constraint that subsequent post-colonial policies (reserved seats, administrative scheduling) inherit and complicate. The framework's ε-invariance principle requires separate stories for the colonial-era imposition (extractiveness rising with entrenchment, 0.42→0.62) versus post-colonial categorical persistence (extractiveness stabilizing around 0.54-0.58 as formal discrimination was prohibited but legacies persist). The shared constraint identity is 'categorical identity assignment'; the distinct constraint stories decompose into 'colonial imposition of categorical fixity' (upstream, high extraction during 1871-1947 period, then declining) and 'post-colonial persistence of categorical legacies' (downstream, stable moderate extraction 1951-2024). Linking through network.affects_constraints preserves the causal and institutional kinship while respecting the ε-invariance requirement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(colonial_caste_categorization_artifact, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
