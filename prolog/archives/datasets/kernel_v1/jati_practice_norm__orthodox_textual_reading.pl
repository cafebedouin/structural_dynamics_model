% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__orthodox_textual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__orthodox_textual_reading, []).

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
 *   constraint_id: jati_practice_norm__orthodox_textual_reading
 *   human_readable: Jati Boundaries as Fixed Scriptural Varna Framework (Orthodox Textual Reading)
 *   domain: social_anthropology/religious_studies/political_economy
 *
 * SUMMARY:
 *   The jati system in South Asia organizes occupational, ritual, and social
 *   identity across complex hierarchical categories. This story captures ONE
 *   reading of that system: the orthodox brahmanical textual reading, which
 *   claims that jati categories derive from and must align with the varna
 *   framework (brahmin, kshatriya, vaishya, shudra plus
 *   achyuta/untouchables), that this alignment is cosmically ordained and
 *   karmically immutable, and that ritual pollution (asuddhi) attaches to
 *   certain occupational categories as a metaphysical fact requiring
 *   brahmanical adjudication. Under this reading, jati boundaries are fixed
 *   by sacred text (Rigveda, Manusmriti, Bhagavad Gita), and deviation from
 *   prescribed varna-occupational alignment constitutes ritual transgression.
 *   The constraint exhibits high extractiveness (0.68) and high suppression
 *   (0.78) because the framework assigns certain jatis to polluting
 *   occupations with no structural exit path, enforces these assignments
 *   through ritual authority and social ostracism, and benefits brahmanical
 *   and upper-varna privileged groups. The theater ratio has risen over the
 *   300-year interval (0.42 → 0.55) as brahmanical textual authority has
 *   shifted from hegemonic to contested — the invocation of varna fixity
 *   increasingly sounds performative (defensive) rather than self-evident
 *   (foundational). This reading coexists with two sibling readings: a
 *   localized-practice reading (emphasizing how jati communities themselves
 *   negotiate boundaries and status, often diverging from brahmanical textual
 *   prescriptions) and a colonial-census reading (examining how British
 *   enumeration reified and standardized varna-jati alignment). Each sibling
 *   reading produces different extractiveness values and authority
 *   structures; together they reveal that the 'cosmic fixity' framing
 *   naturalizes what is actually a contingent institutional arrangement.
 *
 * KEY AGENTS:
 *   - Brahmanical Authority Structure (institutional/arbitrage): Custodian of vedic text, arbiter of ritual correctness, benefits from varna framework legitimacy
 *   - Upper-Varna Privileged Groups (powerful/arbitrage): Brahmins and kshatriyas who hold ritual, educational, and political monopolies; extraction mechanisms work in their favor
 *   - Occupationally-Assigned Jatis with Polluting Roles (powerless/trapped): Jatis assigned to leather work, waste removal, death-related tasks; face categorical pollution blocking mobility and social participation
 *   - Intermediate Jati Communities (moderate/constrained): Artisan and merchant jatis that occupy middle ground — benefit from occupational coordination but constrained by status subordination
 *   - Post-Colonial State (institutional/arbitrage): Formally repudiates varna hierarchy; yet bureaucratic practice and local enforcement persist
 *   - Analytical Observer (analytical/analytical): Risk of naturalizing brahmanical ideology as cosmic law; requires cross-position analysis to reveal institutional contingency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__orthodox_textual_reading, 0.68).
domain_priors:suppression_score(jati_practice_norm__orthodox_textual_reading, 0.78).
domain_priors:theater_ratio(jati_practice_norm__orthodox_textual_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__orthodox_textual_reading, snare).
narrative_ontology:human_readable(jati_practice_norm__orthodox_textual_reading, "Jati Boundaries as Fixed Scriptural Varna Framework (Orthodox Textual Reading)").
narrative_ontology:topic_domain(jati_practice_norm__orthodox_textual_reading, "social_anthropology/religious_studies/political_economy").

domain_priors:requires_active_enforcement(jati_practice_norm__orthodox_textual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__orthodox_textual_reading, 'b6d07a11-526b-48b9-8880-3640c85f713b').
narrative_ontology:cs_kernel_codification('b6d07a11-526b-48b9-8880-3640c85f713b', fixed_text).
narrative_ontology:cs_authority_grounding('b6d07a11-526b-48b9-8880-3640c85f713b', extraction).
narrative_ontology:cs_interpretation_layer_present('b6d07a11-526b-48b9-8880-3640c85f713b').
narrative_ontology:cs_reading_relation('b6d07a11-526b-48b9-8880-3640c85f713b', jati_practice_norm__localized_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('b6d07a11-526b-48b9-8880-3640c85f713b', jati_practice_norm__colonial_census_reading, influences).
narrative_ontology:cs_axiom('b6d07a11-526b-48b9-8880-3640c85f713b', foundational, varna_categories_cosmic_immutable).
narrative_ontology:cs_axiom_status(varna_categories_cosmic_immutable, holdable).
narrative_ontology:cs_axiom_grounding('b6d07a11-526b-48b9-8880-3640c85f713b', varna_categories_cosmic_immutable, deontological).
narrative_ontology:cs_axiom('b6d07a11-526b-48b9-8880-3640c85f713b', foundational, ritual_pollution_metaphysical_fact).
narrative_ontology:cs_axiom_status(ritual_pollution_metaphysical_fact, holdable).
narrative_ontology:cs_axiom_grounding('b6d07a11-526b-48b9-8880-3640c85f713b', ritual_pollution_metaphysical_fact, deontological).
narrative_ontology:cs_axiom('b6d07a11-526b-48b9-8880-3640c85f713b', secondary, brahmanical_authority_uniquely_competent_adjudicate_varna).
narrative_ontology:cs_axiom_status(brahmanical_authority_uniquely_competent_adjudicate_varna, holdable).
narrative_ontology:cs_axiom_grounding('b6d07a11-526b-48b9-8880-3640c85f713b', brahmanical_authority_uniquely_competent_adjudicate_varna, conventional).
narrative_ontology:cs_reference_frame('b6d07a11-526b-48b9-8880-3640c85f713b', vedic_cosmic_order_immutable_varna_categories).
narrative_ontology:cs_drift_state('b6d07a11-526b-48b9-8880-3640c85f713b', contemporary_constitutional_repudiation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b6d07a11-526b-48b9-8880-3640c85f713b', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(jati_practice_norm__orthodox_textual_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, brahmanical_authority_structure).
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, upper_varna_privileged_groups).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, occupational_jati_assigned_polluting_roles).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, mobility_blocked_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OCCUPATIONALLY-ASSIGNED JATI WITH POLLUTING ROLES (SNARE) — Jatis assigned to leather work, waste removal, menial labor, or death-related tasks face categorical ritual pollution that blocks mobility, cross-jati marriage, social commensality, and access to temples. Orthodox textual framework treats this assignment as cosmically inscribed (varna birth determines dharmic duty). Exit is structurally impossible — the jati assignment is not changeable within a single lifetime, ritual pollution attaches to the occupational category itself, and brahmanical authority enforces prohibition against mobility. High suppression through ritual law and social ostracism.
constraint_indexing:constraint_classification(jati_practice_norm__orthodox_textual_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: BRAHMANICAL AUTHORITY STRUCTURE (ROPE) — The orthodox textual reading treats varna categories as fixed cosmic categories grounded in sacred text (Rigveda hymn of Purusha, Manusmriti, Bhagavad Gita). Brahmanical authorities adjudicate ritual correctness and control access to ritual status. From this perspective, the constraint is coordination: the varna framework organizes society according to dharma (duty), coordinates occupational roles with cosmic order, and enables brahmanical ritual function. The authority benefits from the framework's stability — it legitimizes brahmanical epistemic privilege and ritual monopoly. Low extraction from this perspective; the structure is experienced as a legitimate coordination mechanism.
constraint_indexing:constraint_classification(jati_practice_norm__orthodox_textual_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: INTERMEDIATE JATI COMMUNITIES (ARTISAN/MERCHANT GROUPS) (TANGLED ROPE) — Jatis with occupational specialization (weaving, metalwork, commerce) that do not carry ritual pollution status experience mixed coordination and extraction. The varna framework coordinates occupational identity and enables guild-like collective action; members benefit from jati-based mutual aid and occupational specialization. But the framework also constrains mobility, restricts marriage alliance patterns, and subordinates non-brahmanical knowledge systems to brahmanical ritual hierarchy. Constrained exit — these communities can negotiate status (historically many did), but shifting varna placement carries reputational cost and requires brahmanical authority recognition.
constraint_indexing:constraint_classification(jati_practice_norm__orthodox_textual_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / NATURALIZING PERSPECTIVE (MOUNTAIN) — The orthodox textual reading claims that varna categories are cosmic/karmic realities, not social constructions. From a naturalizing lens, the jati-varna alignment appears as an immutable natural law: birth-determined occupational and ritual status is presented as following from karma (accumulated merit/demerit from past lives), cosmic order (rita), and divine decree. The constraint appears unchangeable in principle because it is grounded in metaphysical law, not institutional convention. However, this perspective risks being a false summit — the 'cosmic fixity' framing naturalizes what is actually a contingent institutional arrangement maintained by brahmanical authority and enforceable social sanctions.
constraint_indexing:constraint_classification(jati_practice_norm__orthodox_textual_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: UPPER-VARNA PRIVILEGED GROUPS (BRAHMIN/KSHATRIYA) (SNARE FROM EXTRACTIVE BENEFICIARY POSITION) — While the brahmanical authority structure experiences the framework as rope (coordination), upper-varna privileged groups with exit options (arbitrage) experience it as snare — they are beneficiaries of a system that extracts from lower jatis. The constraint benefits them through monopoly on ritual authority (brahmins), political-military power (kshatriyas), and access to education and mobility. The high extractiveness and suppression mechanisms that harm lower jatis simultaneously benefit upper jatis. The snare classification from this position captures that the extraction mechanism is necessary for their privilege — they have strong incentive to maintain suppression mechanisms and deny alternative mobility pathways.
constraint_indexing:constraint_classification(jati_practice_norm__orthodox_textual_reading, snare,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: POST-COLONIAL STATE ADMINISTRATIVE APPARATUS (PITON) — The modern Indian state (constitutionally secular, with affirmative action provisions for scheduled castes) formally repudiates the varna-jati hierarchy as the basis for rights and resource allocation. Yet bureaucratic practice, census categories inherited from colonial enumeration, and local enforcement of jati-based restrictions persist. The orthodox textual reading persists in ritual and social practice despite constitutional prohibition. The constraint appears as piton: the performative invocation of varna/jati categories continues in daily life and ritual contexts, but the primary institutional legitimacy (the state) has withdrawn. Theater ratio reflects that varna justifications are increasingly defensive rather than openly normative — invoked in private/family contexts rather than in public legal or administrative claims.
constraint_indexing:constraint_classification(jati_practice_norm__orthodox_textual_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__orthodox_textual_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(jati_practice_norm__orthodox_textual_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(jati_practice_norm__orthodox_textual_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(jati_practice_norm__orthodox_textual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(jati_practice_norm__orthodox_textual_reading, TR),
    TR >= 0.70.

:- end_tests(jati_practice_norm__orthodox_textual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts from lower jatis through multiple mechanisms: blocked occupational mobility, blocked marriage alliance patterns, ritual pollution that restricts commensality and temple access, and subordination of non-brahmanical knowledge systems. The extraction is enforced through brahmanical authority denial of ritual status and through social sanctions by higher jatis. The extractiveness value reflects that the framework exists primarily to benefit upper jatis and brahmanical authorities, not to solve coordination problems for lower jatis. Suppression (0.78): High. Suppression mechanisms include: (1) Metaphysical/cosmic framing that presents categories as immutable (karmic law, cosmic order) — making exit literally unthinkable within the framework; (2) Ritual enforcement (brahmin refusal of commensality, denial of temple access, pollution prohibitions); (3) Social enforcement (caste sanctions, marriage prohibition); (4) Occupational monopolies that lock jatis into prescribed roles. The 300-year stability of suppression values (0.75 → 0.78) suggests that despite constitutional repudiation and social change, the enforcement mechanisms remain structurally intact, particularly in ritual and family contexts. Theater ratio (0.42 → 0.55): Rising over the interval. The constraint's performative content has increased as brahmanical textual authority has shifted from hegemonic to contested. The invocation of varna cosmic law now sounds like defensive ideology rather than self-evident truth. Upper-caste actors increasingly must actively assert the framework against counter-claims (affirmative action, social reform movements); the constraint requires more theater to maintain authority.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence. Occupationally-assigned jatis (powerless/trapped) experience pure extraction (snare) — the framework offers them no benefits, only restrictions and pollution status. Brahmanical authority (institutional/arbitrage) experiences coordination (rope) — they see the varna framework as legitimate organization of society according to dharma. Intermediate jati communities (moderate/constrained) experience mixed coordination and extraction (tangled rope) — they benefit from occupational coordination but suffer from status subordination. Upper-varna privileged groups (powerful/arbitrage) experience extraction as beneficiaries (snare from their position as extractors). The post-colonial state (institutional/arbitrage) experiences degradation (piton) — the framework is formally repudiated but persists through social practice. The analytical observer risks false naturalization (mountain) — treating the 'cosmic fixity' framing as cosmic law rather than contingent ideology. The perspectival gap reveals that classification is not neutral: the same constraint appears fundamentally different depending on whether you are trapped within it (snare), benefiting from it (rope or snare as beneficiary), constrained by it (tangled rope), or observing it (piton or mountain). The gap cannot be closed by additional data — it is a structural feature of how power operates.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position: their power level, exit options, and relationship to the extraction flow. Powerless agents assigned to polluting occupations (d ≈ 0.95, near full target) experience maximum extractiveness. Brahmanical authorities with arbitrage options (d ≈ 0.05, near full beneficiary) experience negative effective extraction — the constraint subsidizes their position. Intermediate jati communities with constrained exit (d ≈ 0.55, symmetric) experience moderate extraction. The sigmoid function f(d) maps these d values to experienced extractiveness multipliers. The brahmanical authority perspective shows how a beneficiary with institutional power and arbitrage options derives low d and negative chi — they experience the constraint as beneficial coordination. The upper-varna privileged perspective shows how extractors with high power and arbitrage options (beneficiary relationship, d ≈ 0.15) still experience high chi when classified as snare, because the snare classification itself indicates that extraction is the constraint's primary function. The perspectival gap in directionality (powerless victims get d ≈ 0.95; institutional beneficiaries get d ≈ 0.05) is a 19:1 ratio, one of the steepest in the corpus, indicating severe asymmetry in experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy (the paradox that coordination mechanisms can embed extraction, and pure extraction can be defended as coordination) resolves through three observations: (1) The brahmanical authority structure genuinely solves a coordination problem (organizing occupational roles, enabling guild-like collective action, coordinating ritual participation) — from their perspective, the varna framework IS coordination (rope). (2) But this coordination is asymmetric: it benefits some groups (brahmins, upper jatis) far more than others. The benefits flow toward the top; the costs flow toward the bottom. (3) The constraint simultaneously functions as pure extraction for lower jatis (snare) — it offers them no coordination benefit, only restrictions and pollution status. Mandatrophy is resolved by recognizing that mandatrophy is itself the diagnostic signal: when the same constraint appears as rope from one perspective and snare from another, the perspectival gap IS the constraint's structure. The varna framework works as coordination for those it privileges; it works as pure extraction for those it subordinates. The mandate (the dharmic duty, the cosmic law) masks the asymmetry by claiming that the extracted-from groups benefit through karma accumulation and eventual dharmic duty. This is the classic mandatrophy resolution: the claim that subordinated groups 'should want' the extraction because it serves cosmically ordained purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_authority_vs_localized_practice,
    'Does the orthodox textual reading (varna as cosmic, immutable, textually grounded) accurately describe actual jati practice, or does it conflate a brahmanical textual ideology with the heterogeneous, locally-negotiated practices of jati communities?',
    'Ethnographic comparison: oral histories and recorded practice patterns of jati communities in specific regions (Karnataka, Bengal, Gujarat, Tamil Nadu) vs. brahmanical textual prescriptions. Identify where practice diverges from scriptural varna alignment and whether communities actively negotiate status boundaries.',
    'If practice is substantially localized and negotiated: the orthodox textual reading is a false summit (naturalizing one textual tradition as cosmic law). The constraint would decompose into multiple stories per region with different extractiveness values and different authority structures. If practice adheres closely to textual varna framework across regions: the textual reading is structurally adequate, though still potentially a false summit if the textual tradition itself is contingent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_authority_vs_localized_practice, empirical, 'Whether varna textual framework matches actual jati practice patterns across regions').

omega_variable(
    colonial_enumeration_naturalization,
    'To what degree did colonial census enumeration (1872 onwards) reify and standardize the varna-jati relationship, creating the appearance of a fixed textual cosmic order where practice had been more fluid?',
    'Historical comparison: pre-colonial jati boundary records (temple inscriptions, guild documents, local tax records) vs. colonial census categories vs. contemporary practice. Identify shift points where varna and jati became more strictly correlated, and whether this shift correlates with colonial administrative categories.',
    'If colonial enumeration substantially naturalized the varna-jati link: the ''fixed cosmic framework'' is partly a colonial artifact layered onto brahmanical text layered onto localized practice. The constraint family would include separate stories for pre-colonial, colonial, and post-colonial readings, each with different extractiveness and authority structures. This would reveal that the orthodox textual reading conflates brahmanical ideology with colonial bureaucracy with contemporary practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_enumeration_naturalization, empirical, 'Whether colonial census reified varna-jati relationship').

omega_variable(
    ritual_pollution_mechanism_enforcement,
    'Is ritual pollution (asuddhi) enforced primarily through brahmanical authority coercion (explicit prohibitions, denial of ritual access), or through internalized community norms, or through material resource barriers (occupational monopolies), or through some combination?',
    'Analysis of enforcement mechanisms in specific jati communities: identify where prohibition is explicit (brahmin refusal of commensality, temple access restrictions) vs. internalized (community members internalize pollution status) vs. material (occupational licensing, land access). Compare enforcement strength across regions and time periods.',
    'If enforcement is primarily coercive (brahmanical authority): suppression value 0.78 is accurate; the constraint is snare. If enforcement is primarily internalized: the constraint shows identity-lock dynamics; some perspectives would use identity_locked exit. If enforcement is primarily material (occupational monopolies): the constraint is more complex, mixing resource extraction with ritual ideology. Different mechanisms imply different omega structures for potential change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_pollution_mechanism_enforcement, empirical, 'Primary mechanism of ritual pollution enforcement').

omega_variable(
    natural_law_vs_constructed_claim,
    'Is the varna framework (as presented in orthodox textual reading) a genuine cosmic/karmic natural law, or is it a contingent institutional arrangement that benefits brahmanical authorities and is naturalized through appeals to sacred text and cosmic order?',
    'Meta-analysis: comparison of textual claims to empirical diversity of jati arrangements across South Asian regions and time periods. Identification of counter-examples or alternative textual traditions that reject varna fixity. Assessment of whether the framework''s claimed immutability is defended through logical/metaphysical argument or through social enforcement.',
    'If cosmic law: mountain classification is accurate, and the constraint is genuinely irreducible. If contingent institutional arrangement: mountain is false summit; the true classification is snare (pure extraction) or tangled_rope (mixed). This omega directly determines whether the engine''s false summit detector will reclassify this constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_claim, conceptual, 'Whether varna framework is cosmic natural law or contingent institutional arrangement').

omega_variable(
    brahmanical_authority_identity_lock,
    'Do brahmanical authorities (pandits, temple priests, custodians of textual tradition) experience their role as identity-locked within the varna framework — such that questioning varna fixity would require abandoning professional identity — or do they experience it as a strategic choice to maintain institutional power?',
    'Analysis of brahmanical intellectual history: instances of brahmins challenging varna orthodoxy (Rammohan Roy, Jotirao Phule''s brahmin contemporaries, modern brahmin scholars rejecting varna) vs. those defending orthodoxy. Assessment of whether defense is articulated as ''duty bound by cosmic law'' (identity-lock) or ''this system serves brahmanical interest'' (strategic choice).',
    'If identity-locked: brahmanical defenders are trapped by their own identity frame; change requires their identity reconstitution. If strategic: defenders have exit options but choose not to exercise them; the constraint is maintained through deliberate authority use. Different mechanisms imply different intervention points for change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brahmanical_authority_identity_lock, conceptual, 'Whether brahmanical authorities are identity-locked or strategically defending varna framework').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__orthodox_textual_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_orth_theater_t0, jati_practice_norm__orthodox_textual_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(jati_orth_theater_t150, jati_practice_norm__orthodox_textual_reading, theater_ratio, 150, 0.48).
narrative_ontology:measurement(jati_orth_theater_t300, jati_practice_norm__orthodox_textual_reading, theater_ratio, 300, 0.55).

% Extraction over time
narrative_ontology:measurement(jati_orth_extract_t0, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(jati_orth_extract_t150, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 150, 0.68).
narrative_ontology:measurement(jati_orth_extract_t300, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 300, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(jati_orth_suppress_t0, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(jati_orth_suppress_t150, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 150, 0.77).
narrative_ontology:measurement(jati_orth_suppress_t300, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 300, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__orthodox_textual_reading, identity_coordination).
narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, jati_practice_norm__localized_practice_reading).
narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, jati_practice_norm__colonial_census_reading).

% DUAL FORMULATION NOTE:
% The jati_practice_norm kernel decomposes into three constraint stories, one per reading. Each story has different ε values reflecting the extractiveness of that particular reading's framing: orthodox_textual_reading (ε=0.68, highest, because cosmic fixity maximizes suppression); localized_practice_reading (ε=0.45-0.50, lower, because reveals negotiability); colonial_census_reading (ε=0.55-0.60, intermediate, because layers ideology with bureaucracy). The three readings coexist in contemporary South Asian society — brahmanical authorities defend the textual reading, jati communities enact the localized reading, post-colonial state inherits colonial categorical machinery. The network structure enables analysis of how the kernel itself is contested and how different readings produce different extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jati_practice_norm__orthodox_textual_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
