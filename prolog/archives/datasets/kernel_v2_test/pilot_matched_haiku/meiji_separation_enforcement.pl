% ============================================================================
% CONSTRAINT STORY: meiji_separation_enforcement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_meiji_separation_enforcement, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: meiji_separation_enforcement
 *   human_readable: Meiji Separation Enforcement: State Redefinition of Authentic Shinto
 *   domain: religious_studies/japanese_history/ontology_of_practice
 *
 * SUMMARY:
 *   The Meiji Separation (Shinbutsu Bunri, 1868-1874) represents a
 *   state-enforced ontological redefinition that destroyed centuries of
 *   integrated kami-buddha veneration across Japan. The state apparatus
 *   issued separation edicts, confiscated temple property, reclassified
 *   clergy, and destroyed jingu-ji (shrine-temple complexes) to construct
 *   'authentic' Shinto as a non-Buddhist, nationalist ideology. This
 *   constraint exhibits the full range of DR classification from different
 *   structural positions. The syncretic practitioner experiences pure
 *   extraction (snare) — trapped by geography and institutional dependence,
 *   forced to abandon integrated practice. Buddhist clergy experience
 *   institutional destruction (snare) — loss of interpretive authority and
 *   property. The Meiji state experiences coordination (rope) — separation
 *   enables state religious authority and nationalist consolidation.
 *   Organized Shinto institutions experience mixed coordination-extraction
 *   (tangled rope) — state patronage alongside doctrinal constraint. The
 *   separation edict apparatus becomes increasingly performative (piton) —
 *   enforcement persists through institutional inertia after the primary
 *   extraction is achieved. The analytical observer risks naturalizing the
 *   separation as logical necessity (false summit mountain) — treating
 *   state-imposed ontological boundaries as inherent logical laws. The
 *   constraint's extractiveness rises sharply during the enforcement period
 *   (0.45 → 0.71) as edicts are issued and temples destroyed, then stabilizes
 *   as the separation becomes institutionalized. Suppression peaks during
 *   active enforcement (0.78 at year 2) then declines as resistance is
 *   suppressed and compliance becomes normalized. Theater ratio rises
 *   throughout the interval (0.35 → 0.62) as the apparatus shifts from active
 *   enforcement to performative maintenance.
 *
 * KEY AGENTS:
 *   - Syncretic Practitioners and Institutions: Primary victim (powerless/trapped) — village priests, lay practitioners, jingu-ji complexes bearing full cost of forced reclassification and institutional destruction
 *   - Buddhist Clergy: Secondary victim (moderate/constrained) — institutional authority holders losing interpretive power, property, and doctrinal legitimacy; some exit possible through conversion but at severe cost
 *   - Meiji State Nationalist Project: Primary beneficiary (institutional/arbitrage) — consolidates religious authority, eliminates competing institutional power, constructs nationalist ideology; high agency and exit options
 *   - Shinto Institutional Reorganization: Secondary beneficiary (organized/constrained) — gains state patronage and institutional consolidation; constrained by doctrinal orthodoxy and state oversight
 *   - Separation Edict Apparatus: Institutional actor (institutional/arbitrage) — maintains performative enforcement machinery; sees own process as degraded but continues through inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing state-imposed ontology as logical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(meiji_separation_enforcement, 0.68).
domain_priors:suppression_score(meiji_separation_enforcement, 0.72).
domain_priors:theater_ratio(meiji_separation_enforcement, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(meiji_separation_enforcement, extractiveness, 0.68).
narrative_ontology:constraint_metric(meiji_separation_enforcement, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(meiji_separation_enforcement, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(meiji_separation_enforcement, snare).
narrative_ontology:human_readable(meiji_separation_enforcement, "Meiji Separation Enforcement: State Redefinition of Authentic Shinto").
narrative_ontology:topic_domain(meiji_separation_enforcement, "religious_studies/japanese_history/ontology_of_practice").

domain_priors:requires_active_enforcement(meiji_separation_enforcement).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(meiji_separation_enforcement, '424b6ae0-e3fc-44f1-8b33-4b830a05ceaf').
narrative_ontology:cs_kernel_codification('424b6ae0-e3fc-44f1-8b33-4b830a05ceaf', fixed_text).
narrative_ontology:cs_authority_grounding('424b6ae0-e3fc-44f1-8b33-4b830a05ceaf', extraction).
narrative_ontology:cs_interpretation_layer_present('424b6ae0-e3fc-44f1-8b33-4b830a05ceaf').
narrative_ontology:cs_reading_relation('424b6ae0-e3fc-44f1-8b33-4b830a05ceaf', meiji_separation_enforcement__domain_partition_reading, forecloses).
narrative_ontology:cs_reading_relation('424b6ae0-e3fc-44f1-8b33-4b830a05ceaf', meiji_separation_enforcement__pragmatic_incoherence_reading, forecloses).
narrative_ontology:cs_axiom('424b6ae0-e3fc-44f1-8b33-4b830a05ceaf', foundational, kami_buddha_ontological_unity).
narrative_ontology:cs_axiom_status(kami_buddha_ontological_unity, overridden).
narrative_ontology:cs_axiom_grounding('424b6ae0-e3fc-44f1-8b33-4b830a05ceaf', kami_buddha_ontological_unity, deontological).
narrative_ontology:cs_axiom('424b6ae0-e3fc-44f1-8b33-4b830a05ceaf', foundational, honji_suijaku_doctrinal_authority).
narrative_ontology:cs_axiom_status(honji_suijaku_doctrinal_authority, overridden).
narrative_ontology:cs_axiom_grounding('424b6ae0-e3fc-44f1-8b33-4b830a05ceaf', honji_suijaku_doctrinal_authority, conventional).
narrative_ontology:cs_axiom('424b6ae0-e3fc-44f1-8b33-4b830a05ceaf', foundational, state_religious_authority_supremacy).
narrative_ontology:cs_axiom_status(state_religious_authority_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('424b6ae0-e3fc-44f1-8b33-4b830a05ceaf', state_religious_authority_supremacy, conventional).
narrative_ontology:cs_axiom('424b6ae0-e3fc-44f1-8b33-4b830a05ceaf', foundational, shinto_authenticity_non_buddhist).
narrative_ontology:cs_axiom_status(shinto_authenticity_non_buddhist, holdable).
narrative_ontology:cs_axiom_grounding('424b6ae0-e3fc-44f1-8b33-4b830a05ceaf', shinto_authenticity_non_buddhist, empirically_contingent).
narrative_ontology:cs_reference_frame('424b6ae0-e3fc-44f1-8b33-4b830a05ceaf', syncretic_fusion_reading).
narrative_ontology:cs_drift_state('424b6ae0-e3fc-44f1-8b33-4b830a05ceaf', meiji_enforcement_period, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('424b6ae0-e3fc-44f1-8b33-4b830a05ceaf', '2026-02-26T14:32:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(meiji_separation_enforcement, meiji_state_nationalist_project).
narrative_ontology:constraint_victim(meiji_separation_enforcement, syncretic_practitioners_and_institutions).
narrative_ontology:constraint_victim(meiji_separation_enforcement, buddhist_institutional_authority).
narrative_ontology:constraint_victim(meiji_separation_enforcement, folk_ritual_autonomy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(meiji_separation_enforcement, shinto_institutional_reorganization).
narrative_ontology:constraint_victim(meiji_separation_enforcement, syncretic_practitioners).
narrative_ontology:constraint_victim(meiji_separation_enforcement, jingu_ji_institutions).
narrative_ontology:constraint_victim(meiji_separation_enforcement, buddhist_clergy).
narrative_ontology:constraint_victim(meiji_separation_enforcement, shinto_institutional_reorganization).
narrative_ontology:constraint_vindicates(meiji_separation_enforcement, shinto_authenticity_doctrine).
narrative_ontology:constraint_vindicates(meiji_separation_enforcement, state_religious_authority_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Village priests and lay practitioners who maintained integrated kami-buddha veneration face forced reclassification. They must choose between abandoning ancestral practice, converting to state-approved Shinto priesthood, or losing institutional affiliation entirely. Geographic location and institutional dependence make exit impossible without abandoning identity.
narrative_ontology:constraint_stakeholder(meiji_separation_enforcement, syncretic_practitioners, payer,
    powerless, biographical, trapped, national).

% Shrine-temple complexes that integrated kami and buddha veneration face destruction or forced conversion. Some institutions survive by converting to pure Shinto shrines or pure Buddhist temples, but this requires abandoning their integrated function and identity. Property confiscation and institutional restructuring impose severe costs.
narrative_ontology:constraint_stakeholder(meiji_separation_enforcement, jingu_ji_institutions, payer,
    moderate, biographical, constrained, national).

% Buddhist priests who grounded authority in honji-suijaku doctrine face state-mandated doctrinal erasure. They can convert to Shinto priesthood, become secular scholars, or maintain Buddhist practice in reduced institutional form, but all options involve severe career and status loss. Institutional property is confiscated and doctrinal authority is eliminated.
narrative_ontology:constraint_stakeholder(meiji_separation_enforcement, buddhist_clergy, payer,
    moderate, biographical, constrained, national).

% The state apparatus issues separation edicts, confiscates temple property, reclassifies clergy, and destroys jingu-ji complexes. The state benefits from consolidating religious authority under state control, eliminating competing institutional power (Buddhist hierarchy), and constructing 'authentic' Shinto as nationalist ideology. The state has high agency and can revise edicts or redefine categories as needed.
narrative_ontology:constraint_stakeholder(meiji_separation_enforcement, meiji_state_apparatus, agenda_setter,
    institutional, immediate, arbitrage, national).

% Organized Shinto institutions (shrine associations, priestly training systems) benefit from state patronage and institutional consolidation. They gain resources, official recognition, and institutional infrastructure. However, they are constrained by state oversight, doctrinal orthodoxy requirements, and loss of syncretic flexibility. They must maintain state-approved doctrinal boundaries and cannot revert to integrated practice.
narrative_ontology:constraint_stakeholder(meiji_separation_enforcement, shinto_institutional_reorganization, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(meiji_separation_enforcement, shinto_institutional_reorganization, payer).

% The capacity for local communities to maintain integrated kami-buddha veneration without state-imposed doctrinal boundaries is suppressed. Folk practice is constrained by state enforcement, institutional destruction, and clergy reclassification. The abstract collective good of ritual autonomy is eliminated.
narrative_ontology:constraint_stakeholder(meiji_separation_enforcement, folk_ritual_autonomy, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_non_agent(meiji_separation_enforcement, folk_ritual_autonomy).

% The integrated kami-buddha ontological framework (honji-suijaku doctrine and syncretic practice) is destroyed by state enforcement. The cosmological system that unified kami as local manifestations of universal buddha-nature is replaced by state-imposed ontological separation. The framework cannot be recovered once institutional infrastructure is destroyed.
narrative_ontology:constraint_stakeholder(meiji_separation_enforcement, syncretic_cosmology, payer,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(meiji_separation_enforcement, syncretic_cosmology).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint coordinates religious boundary definition: the state establishes clear categorical separation between Shinto (kami veneration) and Buddhism (buddha veneration) to enable nationalist consolidation and state religious authority. The coordination problem is: how can the state establish unified religious categories for nationalist ideology when centuries of integrated practice have blurred boundaries?
% TRANSFER_FUNCTION: The constraint transfers religious authority from Buddhist institutional hierarchy (honji-suijaku doctrine) to state-imposed definition. It transfers temple property from Buddhist institutions to the state or to reorganized Shinto shrines. It transfers interpretive authority from Buddhist clergy to state-appointed Shinto priests. It transfers ritual autonomy from local communities to state-regulated shrine systems.
% ABSENT_VOICES: Folk practitioners who maintained syncretic practice outside institutional frameworks are excluded from the separation edicts — they are not consulted about doctrinal redefinition. Buddhist scholars who developed honji-suijaku doctrine are excluded from the state's authority structure. Practitioners in remote areas who continued integrated veneration without state oversight are absent from official records. The constraint is imposed without consent from those most affected.
% DISAPPEARANCE_RATIONALE: If the separation enforcement disappeared, the world would rearrange itself: Buddhist institutions would recover confiscated property and doctrinal authority, syncretic practice would resume in shrine-temple complexes, and the state's nationalist religious ideology would lose institutional foundation. The separation is not a natural fact but a contingent institutional arrangement that depends on continuous state enforcement.
% FOUNDING_PROBLEM: The founding problem is the state's need to establish unified religious categories for nationalist consolidation. The Meiji state required clear separation between Shinto (as nationalist ideology) and Buddhism (as foreign/cosmopolitan) to construct a coherent national identity. The centuries of integrated kami-buddha veneration created ambiguity that threatened the state's nationalist project.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (state need for religious boundary definition) was live during the Meiji period (1868-1912) when nationalist consolidation was the state's primary agenda. By the early 20th century, the separation was institutionalized and the state's nationalist ideology was established. Contemporary sources (Meiji government documents, Shinto institutional records) attest that the founding problem was live during the enforcement period. However, by the 1920s-1930s, the founding problem was largely resolved — the separation was normalized and the state's religious authority was established. The constraint persists beyond the founding problem's resolution, indicating mandatrophy: the apparatus maintains separation through institutional inertia rather than through active response to the founding problem.
narrative_ontology:disappearance_verdict(meiji_separation_enforcement, world_rearranges).
narrative_ontology:founding_problem_status(meiji_separation_enforcement, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SYNCRETIC PRACTITIONER (SNARE) — Village priests and lay practitioners who maintained integrated kami-buddha veneration face forced reclassification, temple destruction, and identity erasure. Trapped by geographic location and institutional dependence; cannot exit without abandoning ancestral practice. Maximum extraction: coerced choice between doctrinal abandonment or institutional elimination.
constraint_indexing:constraint_classification(meiji_separation_enforcement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BUDDHIST CLERGY (SNARE) — Institutional authority holders who grounded legitimacy in honji-suijaku doctrine face state-mandated doctrinal erasure. Constrained by career dependence and institutional property loss; some exit possible through conversion to Shinto priesthood or secular roles, but at severe cost. Extraction: loss of interpretive authority, property confiscation, forced institutional restructuring.
constraint_indexing:constraint_classification(meiji_separation_enforcement, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MEIJI STATE NATIONALIST PROJECT (ROPE) — State apparatus benefits from separation enforcement: consolidates religious authority under state control, eliminates competing institutional power (Buddhist hierarchy), and constructs 'authentic' Shinto as nationalist ideology. Experiences constraint as coordination mechanism: defining religious boundaries enables state-sponsored modernization and imperial legitimacy. Net beneficiary with high agency and exit options (can revise edicts, redefine categories).
constraint_indexing:constraint_classification(meiji_separation_enforcement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SHINTO INSTITUTIONAL REORGANIZATION (TANGLED ROPE) — Organized Shinto institutions (shrine associations, priestly training systems) benefit from state patronage and institutional consolidation while bearing costs of doctrinal constraint and loss of syncretic flexibility. Constrained by state oversight and doctrinal orthodoxy requirements; some agency through institutional adaptation and reinterpretation. Mixed extraction: gains institutional resources and state support; loses doctrinal autonomy and ritual flexibility.
constraint_indexing:constraint_classification(meiji_separation_enforcement, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: SEPARATION EDICT APPARATUS (PITON) — The bureaucratic machinery of separation enforcement (edict publication, temple inventory, clergy reclassification records) persists as institutional theater long after the primary extraction mechanism (doctrinal redefinition) has achieved its goal. Theater ratio reflects that enforcement becomes increasingly performative: edicts are reissued, compliance is monitored, but the underlying ontological claim (authentic Shinto is non-Buddhist) is maintained through institutional inertia rather than active suppression. The apparatus is degraded — it maintains the separation through ritual compliance rather than through genuine doctrinal enforcement.
constraint_indexing:constraint_classification(meiji_separation_enforcement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, religious ontologies are inherently bounded and mutually exclusive: kami and buddhas cannot simultaneously occupy the same cosmological space without contradiction. This perspective sees the separation as inevitable clarification of what was always logically incoherent. However, the structural data contradicts this classification — the engine will compute this as a false summit, revealing that the 'logical necessity' framing naturalizes what is actually a state-imposed ontological redefinition.
constraint_indexing:constraint_classification(meiji_separation_enforcement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(meiji_separation_enforcement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(meiji_separation_enforcement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(meiji_separation_enforcement, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(meiji_separation_enforcement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(meiji_separation_enforcement, TR),
    TR >= 0.70.

:- end_tests(meiji_separation_enforcement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The state extracts substantial benefit from separation enforcement: consolidates religious authority, eliminates competing institutional power (Buddhist hierarchy), constructs nationalist ideology, and confiscates temple property. The extraction is not maximal (0.72) because some institutional actors (reorganized Shinto) benefit from state patronage, creating a mixed extraction profile. Suppression (0.72): High. Significant barriers to resistance include state military/police power, property confiscation, clergy reclassification, and destruction of institutional infrastructure. Suppression is not total because some syncretic practice persists in folk contexts and some Buddhist institutions survive through adaptation. Theater ratio (0.58): Moderate-high. The separation edicts are partly functional (they do accomplish doctrinal redefinition and institutional reorganization) but increasingly performative as enforcement becomes normalized. The apparatus maintains separation through ritual compliance (continued edict reissuance, clergy monitoring) rather than through active suppression after the initial enforcement period.
 *
 * PERSPECTIVAL GAP:
 *   The syncretic practitioner and Buddhist clergy experience snare classification because they are trapped targets of extraction. The Meiji state experiences rope classification because they are beneficiaries solving a coordination problem (defining religious boundaries). Organized Shinto experiences tangled rope because they are mixed beneficiaries and victims. The separation apparatus experiences piton because enforcement is performative. The analytical observer risks false summit mountain because they naturalize state-imposed ontology as logical necessity. The gap reveals that the constraint's type is not intrinsic but perspectival — it depends on the observer's structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive directionality derivation. The Meiji state is declared beneficiary — they consolidate religious authority and construct nationalist ideology. Syncretic practitioners and institutions are declared victims — they lose integrated practice and institutional autonomy. Buddhist clergy are declared victims — they lose interpretive authority and property. Folk ritual autonomy is declared victim — the state suppresses alternative authority structures. The engine derives d from these declarations plus power/exit: beneficiaries with arbitrage exit get low d (negative χ); victims with trapped exit get high d (high χ). The directionality overrides are not needed — the structural derivation captures the true relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The separation enforcement constraint resolves the mandatrophy by showing that the state's mandate (define authentic Shinto for nationalist consolidation) has outlived its original function (religious boundary clarification) and persists through institutional inertia. The mandate was live during the enforcement period (1868-1874) when active suppression was necessary to overcome resistance. By year 6, the mandate persists but the primary extraction mechanism (forced reclassification) has achieved its goal — the separation is now maintained through performative apparatus (theater ratio rising to 0.62) rather than through active suppression (suppression declining to 0.65). The constraint transitions from snare (active extraction) to piton (performative maintenance) as the apparatus becomes degraded. The mandatrophy is resolved by recognizing that the constraint's function has changed: it is no longer extracting through active suppression but maintaining extraction through institutional inertia. The false summit mountain classification (analytical observer) is a key diagnostic: it reveals that the state's ontological claim (authentic Shinto is non-Buddhist) is naturalized as logical necessity rather than recognized as constructed state authority. This naturalization is itself part of the extraction mechanism — it makes the separation appear inevitable rather than contingent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    syncretic_fusion_vs_pragmatic_incoherence,
    'Did pre-Meiji kami-buddha veneration constitute a unified ontological system (syncretic fusion reading) or an incoherent pragmatic navigation of contradictory frameworks (pragmatic incoherence reading)?',
    'Textual analysis of pre-Meiji doctrinal sources (honji-suijaku theory, Tendai/Shingon scholasticism); ethnographic reconstruction of folk practice patterns; examination of whether practitioners articulated unified cosmology or operated without explicit ontological claims',
    'If unified ontology: the separation was destruction of a coherent system (snare classification confirmed). If pragmatic incoherence: the separation imposed coherence on what was previously flexible (snare classification still holds but for different reason — suppression of flexibility rather than destruction of system).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(syncretic_fusion_vs_pragmatic_incoherence, empirical, 'Whether pre-Meiji kami-buddha veneration was unified ontology or pragmatic incoherence').

omega_variable(
    natural_law_vs_constructed_ontology,
    'Is the mutual exclusivity of kami and buddha ontologies a natural logical law or a constructed state-imposed definition?',
    'Comparative analysis of other religious traditions with multiple sacred categories (Hindu-Buddhist syncretism, Christian-indigenous syncretism); examination of whether logical incompatibility is inherent or whether alternative coherent frameworks could accommodate both kami and buddhas',
    'If natural law: mountain classification is justified (analytical observer correctly identifies logical necessity). If constructed: mountain is a false summit (state-imposed ontology naturalized as logical law).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ontology, conceptual, 'Whether kami-buddha mutual exclusivity is natural law or constructed definition').

omega_variable(
    extraction_mechanism_identification,
    'What is the primary extraction mechanism: doctrinal redefinition (ontological control), institutional property seizure (economic extraction), or suppression of alternative authority structures (political extraction)?',
    'Quantitative analysis of jingu-ji destruction rates vs property confiscation rates vs clergy reclassification rates; temporal sequencing of edicts to identify which mechanism was primary; examination of state revenue flows from confiscated temple property',
    'If doctrinal: snare classification emphasizes ontological suppression. If property seizure: snare classification emphasizes economic extraction. If political: snare classification emphasizes institutional power consolidation. Different mechanisms suggest different victim groups and different suppression profiles.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_mechanism_identification, empirical, 'Primary extraction mechanism in separation enforcement').

omega_variable(
    false_summit_mountain_candidate,
    'Is the analytical observer''s mountain classification a genuine natural law or a false summit naturalizing state-imposed ontology?',
    'Examination of whether the logical incompatibility of kami and buddha ontologies was asserted before Meiji separation or whether it was constructed by the state to justify separation. If pre-existing: mountain is justified. If post-hoc: false summit.',
    'If false summit: the constraint is snare from all perspectives (state-imposed extraction naturalized as logical necessity). If genuine mountain: the separation is clarification of inherent logical boundaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_mountain_candidate, conceptual, 'Whether mountain classification is natural law or false summit').

omega_variable(
    domain_partition_reading_viability,
    'Could the pre-Meiji kami-buddha relationship be coherently understood as domain partition (kami for this-worldly events, buddhas for afterlife) rather than syncretic fusion?',
    'Textual analysis of folk practice patterns and ritual specialization; examination of whether practitioners articulated domain-partition logic or whether this is a retrospective analytical reconstruction',
    'If domain partition was live reading: the separation destroyed a coherent alternative ontology (snare classification confirmed with different structural basis). If domain partition is retrospective: the separation imposed coherence on what was previously flexible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_partition_reading_viability, empirical, 'Whether domain partition reading was coherent pre-Meiji framework').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(meiji_separation_enforcement, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(meiji_sep_tr_t0, meiji_separation_enforcement, theater_ratio, 0, 0.35).
narrative_ontology:measurement(meiji_sep_tr_t2, meiji_separation_enforcement, theater_ratio, 2, 0.48).
narrative_ontology:measurement(meiji_sep_tr_t4, meiji_separation_enforcement, theater_ratio, 4, 0.58).
narrative_ontology:measurement(meiji_sep_tr_t6, meiji_separation_enforcement, theater_ratio, 6, 0.62).

% Extraction over time
narrative_ontology:measurement(meiji_sep_be_t0, meiji_separation_enforcement, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(meiji_sep_be_t2, meiji_separation_enforcement, base_extractiveness, 2, 0.62).
narrative_ontology:measurement(meiji_sep_be_t4, meiji_separation_enforcement, base_extractiveness, 4, 0.71).
narrative_ontology:measurement(meiji_sep_be_t6, meiji_separation_enforcement, base_extractiveness, 6, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(meiji_sep_su_t0, meiji_separation_enforcement, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(meiji_sep_su_t2, meiji_separation_enforcement, suppression_requirement, 2, 0.78).
narrative_ontology:measurement(meiji_sep_su_t4, meiji_separation_enforcement, suppression_requirement, 4, 0.72).
narrative_ontology:measurement(meiji_sep_su_t6, meiji_separation_enforcement, suppression_requirement, 6, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(meiji_separation_enforcement, identity_coordination).
narrative_ontology:affects_constraint(meiji_separation_enforcement, honji_suijaku_doctrine_authority).
narrative_ontology:affects_constraint(meiji_separation_enforcement, buddhist_institutional_power_meiji).
narrative_ontology:affects_constraint(meiji_separation_enforcement, shinto_nationalist_ideology_construction).

% DUAL FORMULATION NOTE:
% The separation enforcement is downstream of the kami-buddha ontology kernel but represents a distinct structural constraint. The upstream kernel has multiple readings (syncretic fusion, domain partition, pragmatic incoherence); the separation enforcement instantiates the state's assertion of the fusion reading's foreclosure. The separation has its own extractiveness value (0.68) reflecting the state's institutional extraction; the kernel readings have different extractiveness values reflecting different authority structures and beneficiary relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
