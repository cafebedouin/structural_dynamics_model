% ============================================================================
% CONSTRAINT STORY: global_ai_training_data_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_global_ai_training_data_asymmetry, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: global_ai_training_data_asymmetry
 *   human_readable: Global AI Training Data Asymmetry
 *   domain: technology/artificial_intelligence/data_economics
 *
 * SUMMARY:
 *   The global AI training data asymmetry represents a structural extraction
 *   mechanism embedded in the contemporary architecture of machine learning
 *   development. Wealthy corporations and well-resourced research
 *   institutions in the Global North source training data from populations in
 *   the Global South, linguistic minorities, and digitally-active subjects
 *   globally, without consent, compensation, or transparent data practices.
 *   The constraint exhibits coordination function (solving the real technical
 *   problem of gathering diverse training data at scale) coupled with
 *   asymmetric extraction (concentrating benefits to wealthy developers while
 *   concentrating costs on powerless data subjects). The extractiveness has
 *   grown as AI models have become larger and more data-hungry; the theater
 *   ratio remains moderate because data sourcing, while opaque, is not
 *   primarily performative — the data is genuinely used in training. Emerging
 *   regulatory frameworks (GDPR, data sovereignty movements, privacy
 *   regulation) and alternative technical approaches (federated learning,
 *   privacy-preserving training, open-source decentralization) represent
 *   sunset mechanisms that could fundamentally alter the extraction mechanism
 *   over a generational timescale.
 *
 * KEY AGENTS:
 *   - Individual Data Subject in Global South: Primary victim (powerless/trapped) — biometric, social, financial, and behavioral data harvested without consent or compensation; no contractual relationship or legal recourse
 *   - Developing Nation Economy: Secondary victim and partial beneficiary (moderate/constrained) — population data extracted while nation benefits from cloud access and open AI models; exit options limited by technological dependency
 *   - Wealthy AI Developer Corporation: Primary beneficiary (institutional/arbitrage) — solves genuine coordination problem of data sourcing; experiences constraint as infrastructure for capability scaling; can arbitrage across jurisdictions and licensing models
 *   - Global South Data Sovereignty Movements: Organized collective (organized/constrained) — building exit pathways through regulation (GDPR-inspired), data unions, community protocols, and compensation schemes; constrained by capital and technical requirements
 *   - Open-Source AI Community: Institutional beneficiary (institutional/constrained) — coordinates on shared model development (genuine value); sustained by asymmetric data pipelines; constrained exit because disengaging breaks coordination function
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing contingent business practice (current corporate data acquisition norms) as technological necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_ai_training_data_asymmetry, 0.58).
domain_priors:suppression_score(global_ai_training_data_asymmetry, 0.65).
domain_priors:theater_ratio(global_ai_training_data_asymmetry, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_ai_training_data_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(global_ai_training_data_asymmetry, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(global_ai_training_data_asymmetry, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(global_ai_training_data_asymmetry, tangled_rope).
narrative_ontology:human_readable(global_ai_training_data_asymmetry, "Global AI Training Data Asymmetry").
narrative_ontology:topic_domain(global_ai_training_data_asymmetry, "technology/artificial_intelligence/data_economics").

domain_priors:requires_active_enforcement(global_ai_training_data_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(global_ai_training_data_asymmetry, wealthy_ai_developers).
narrative_ontology:constraint_beneficiary(global_ai_training_data_asymmetry, northern_hemisphere_tech_corporations).
narrative_ontology:constraint_beneficiary(global_ai_training_data_asymmetry, surveillance_state_actors).
narrative_ontology:constraint_victim(global_ai_training_data_asymmetry, global_south_populations).
narrative_ontology:constraint_victim(global_ai_training_data_asymmetry, individual_data_subjects).
narrative_ontology:constraint_victim(global_ai_training_data_asymmetry, linguistic_minorities).
narrative_ontology:constraint_victim(global_ai_training_data_asymmetry, developing_nation_economies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DATA SUBJECT IN GLOBAL SOUTH (SNARE) — Individual whose digital traces (social media, search, biometric, financial) are harvested without consent, understanding, or compensation. No contractual relationship, no legal recourse across jurisdictional boundaries, no technical means to opt out of global data pipelines. Experiences maximum extraction with no coordination benefit. Cannot exit the digital commons that captures them.
constraint_indexing:constraint_classification(global_ai_training_data_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING NATION ECONOMY (TANGLED ROPE) — Nation-state that benefits from digital infrastructure access (cloud services, AI tools, open models) while simultaneously having its population data extracted for model training without compensation. Genuine coordination (needs access to AI infrastructure) coupled with asymmetric extraction (data flows north without corresponding benefit flow). Exit options constrained by technological dependency and capital requirements for alternative infrastructure.
constraint_indexing:constraint_classification(global_ai_training_data_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: WEALTHY AI DEVELOPER CORPORATION (ROPE) — Tech corporation solving genuine coordination problem: scaling language models, vision systems, and multimodal models requires diverse training data. Data sourcing enables network effects and capability improvements that benefit other developers and users. Experiences constraint as coordination mechanism; extraction accrues to them but is not the constraint's primary function. Can arbitrage data sources, licensing arrangements, and computation across jurisdictions.
constraint_indexing:constraint_classification(global_ai_training_data_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DATA PRIVACY REGULATION & COLLECTIVE ACTION (SCAFFOLD) — GDPR, CCPA, emerging Global South data sovereignty movements, data unions, and federated learning research represent sunset mechanisms for the current asymmetry. Organized collective actors (regulators, advocacy coalitions, alternative AI development movements) are building exit pathways: data compensation schemes, privacy-preserving training, sovereign model development. Extraction will decline as alternative paradigms mature. Sunset estimated at 10-20 years.
constraint_indexing:constraint_classification(global_ai_training_data_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: NORTHERN HEMISPHERE OPEN-SOURCE AI COMMUNITY (TANGLED ROPE) — Community that genuinely coordinates on shared model development and knowledge infrastructure, but also benefits from and sustains the asymmetric data pipeline. Open models democratize AI access, creating coordination value. But the data behind those models is sourced asymmetrically, with extraction concentrated on Global South and vulnerable populations. Constrained exit because disengaging from data pipelines breaks the coordination function.
constraint_indexing:constraint_classification(global_ai_training_data_asymmetry, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — At civilizational scale, the asymmetry reflects a deep structural feature of information economics: training modern AI systems requires massive diverse datasets, and the cost to obtain consent, compensate, and manage rights for billions of data points is prohibitive. From this view, the asymmetry is an inherent consequence of the physics and economics of machine learning at scale — a mountain produced by the coordination requirements of the technology itself. This perspective risks naturalizing what is contingent institutional practice (current corporate data acquisition norms) as technological inevitability.
constraint_indexing:constraint_classification(global_ai_training_data_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_ai_training_data_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(global_ai_training_data_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_ai_training_data_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(global_ai_training_data_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(global_ai_training_data_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint concentrates significant value flows toward wealthy AI developers while imposing costs (privacy loss, behavioral modeling, potential harm from biased models) on data subjects. The extraction is not maximal because genuine coordination value exists — diverse training data does improve model capability and enables useful AI infrastructure. The value increased from 0.35 to 0.58 over the interval as models scaled and data acquisition became more aggressive. Suppression (0.65): High. Multiple barriers prevent exit: (1) technical — data subjects cannot easily identify or opt out of data collection pipelines; (2) legal — inadequate data protection enforcement in jurisdictions where extraction occurs; (3) economic — no compensation mechanisms create economic dependence; (4) informational — lack of transparency about data use and model training. Barriers are highest for Global South populations with limited legal recourse. Theater ratio (0.48): Moderate. Data sourcing and training are functionally genuine — the data is actually used to improve model capability. But significant performative elements exist: (1) corporate sustainability narratives disguising extraction as 'contribution to progress'; (2) regulatory theater (privacy policies that are incomprehensible to subjects); (3) open-source framing that obscures asymmetric sourcing. Theater has grown as regulatory pressure has increased corporate disclosure requirements, some of which serve to make extraction appear transparent while maintaining opacity about actual data flows.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the perspectival gap principle: the same structural reality (asymmetric data sourcing) is classified as Snare by the powerless, Tangled Rope by the moderate, Rope by the beneficiary, and Scaffold by organized actors building exit mechanisms. The analytical observer risks Mountainization — treating the current asymmetry as inevitable when it reflects contingent institutional choices about data acquisition practices, not technological necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness is computed from their structural position: beneficiary status + arbitrage options (wealthy developers) → low d → negative effective extraction; victim status + trapped options (data subjects) → high d → high effective extraction; mixed status + constrained options (moderate agents, institutional beneficiaries in coordinating systems) → moderate d → moderate effective extraction. The pipeline's directional derivation is: d is determined by beneficiary/victim declarations and exit_options; f(d) is the sigmoid applied to d; χ = ε × f(d) × σ(S) produces effective extraction scaled by scope.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY PARTIALLY ACTIVE (ε=0.58, χ to be computed): This constraint requires resolution of the coordination-vs-extraction ambiguity. The tangled rope classification is justified: (1) genuine coordination function exists (solving the technical problem of training diverse models); (2) beneficiaries are identified (wealthy developers, open-source communities); (3) victims are identified (data subjects, Global South populations); (4) active enforcement is required (corporate data sourcing practices). However, the perspective from the powerless data subject classifies the same constraint as a Snare, not Tangled Rope. This is not a classification error — it reflects a real perspectival split. The data subject experiences no coordination benefit; the constraint is pure extraction from their position. The mandatrophy is resolved by accepting that the constraint is Tangled Rope at the meta-level (from the analytical/civilizational view, coordination value is genuine) but appears as Snare to powerless agents (who bear costs without benefits). The classification system captures both readings through perspectival decomposition. The constraint does NOT reduce to pure extraction (Snare) because coordination value is demonstrably real; it does NOT reduce to pure coordination (Rope) because asymmetric extraction is demonstrably real. Tangled Rope is the stable classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_versus_coordination_threshold,
    'At what scale does obtaining informed consent for training data use become technically and economically impossible, vs merely difficult and expensive?',
    'Cost analysis of consent mechanisms (blockchain attestation, cryptographic proof of authorization) at scale; comparison with current corporate acquisition costs; threshold identification where marginal cost to obtain consent exceeds training value.',
    'If threshold is real (e.g., cost exceeds 40% of training budget): mountain classification gains force. If threshold is contingent on current business models: extraction mechanism is choice, not necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_versus_coordination_threshold, empirical, 'Threshold between impossible consent and contingent choice').

omega_variable(
    differential_harm_from_asymmetric_training,
    'Do AI systems trained asymmetrically (biased toward wealthy-nation data) produce measurably different harms for different populations, vs uniform outcome distributions?',
    'Empirical evaluation of model performance across languages, regions, demographics, and geographic contexts; measurement of accuracy gaps, bias amplification, and downstream economic/social harms by population group.',
    'If differential: snare classification strengthened — extraction is selective and targeted. If uniform: tangled rope classification — asymmetric sourcing with symmetric (though problematic) outcomes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(differential_harm_from_asymmetric_training, empirical, 'Whether asymmetric training produces differential population harms').

omega_variable(
    open_source_decentralization_viability,
    'Can open-source, federated, or locally-trained AI models provide capability parity with centrally-trained models without relying on asymmetric data pipelines?',
    'Longitudinal comparison of open-source model capabilities vs proprietary models; tracking of decentralized training progress; measurement of data efficiency improvements in privacy-preserving and federated approaches.',
    'If viable: scaffold sunset is real, extraction mechanism becomes contingent choice. If infeasible: open-source path maintains asymmetry, mountain perspective gains credibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_source_decentralization_viability, empirical, 'Viability of decentralized AI training without asymmetric data').

omega_variable(
    northern_dominance_persistence,
    'Is the concentration of AI capability in wealthy nations a temporary function of current capital/compute distribution, or does it reflect deeper structural asymmetries in access to data, compute, and expertise?',
    'Analysis of capital concentration trends; mapping of compute infrastructure ownership; tracking of skill development and research output by region; measurement of network effects and path dependence in AI ecosystem.',
    'If temporary: tangled rope with genuine path to transition. If structural: snare classification strengthens — extraction mechanism is embedded in global economic asymmetries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(northern_dominance_persistence, conceptual, 'Whether northern AI dominance is temporary or structural').

omega_variable(
    identity_lock_in_data_labor_narratives,
    'To what extent do data subjects internalize narratives of ''free platform participation'' or ''contributing to progress'' that prevent cognitive recognition of extraction?',
    'Qualitative research on data subject awareness and consent comprehension; tracking of discourse shifts as data economy becomes more explicit; measurement of behavioral change when extraction mechanisms are made salient.',
    'If identity-locked: suppression is higher than structural barriers alone suggest — agents carry internalized constraints. Affects classification from powerless perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_data_labor_narratives, empirical, 'Identity-lock in data labor and platform participation narratives').

omega_variable(
    technical_reversibility_of_data_extraction,
    'Can data subjects or their representatives effectively remove or revoke their data from trained models once extraction has occurred, and does such removal degrade model performance meaningfully?',
    'Empirical testing of machine unlearning and data removal techniques; measurement of performance degradation from selective data removal; assessment of computational cost to implement reversibility.',
    'If reversible: constraints on extraction tighten (exit option improves). If irreversible: extraction becomes permanent, snare classification strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_reversibility_of_data_extraction, empirical, 'Whether extracted data can be removed from trained models').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_ai_training_data_asymmetry, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gaitda_tr_t0, global_ai_training_data_asymmetry, theater_ratio, 0, 0.32).
narrative_ontology:measurement(gaitda_tr_t3, global_ai_training_data_asymmetry, theater_ratio, 3, 0.4).
narrative_ontology:measurement(gaitda_tr_t6, global_ai_training_data_asymmetry, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(gaitda_be_t0, global_ai_training_data_asymmetry, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gaitda_be_t3, global_ai_training_data_asymmetry, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(gaitda_be_t6, global_ai_training_data_asymmetry, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(global_ai_training_data_asymmetry, resource_allocation).
narrative_ontology:affects_constraint(global_ai_training_data_asymmetry, algorithmic_bias_global_deployment).
narrative_ontology:affects_constraint(global_ai_training_data_asymmetry, data_labor_exploitation).
narrative_ontology:affects_constraint(global_ai_training_data_asymmetry, language_model_linguistic_colonialism).

% DUAL FORMULATION NOTE:
% The global AI training data asymmetry decomposes into at least three structurally distinct constraints with different ε values: (1) data_sourcing_asymmetry (current story, ε=0.58, Tangled Rope) — the raw extraction from data subjects through opaque data practices; (2) algorithmic_bias_global_deployment (ε=0.62, Snare) — the downstream harm when models trained on asymmetric data are deployed to populations underrepresented in training; (3) data_labor_exploitation (ε=0.72, Snare) — when data sourcing involves explicit labor extraction (content moderation, annotation, platform curation). These constraints are linked: the sourcing asymmetry enables the bias mechanism and sustains the labor exploitation. All three are members of the AI extraction constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(global_ai_training_data_asymmetry, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
