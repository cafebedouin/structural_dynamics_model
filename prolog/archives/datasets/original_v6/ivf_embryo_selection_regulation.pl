% ============================================================================
% CONSTRAINT STORY: ivf_embryo_selection_regulation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ivf_embryo_selection_regulation, []).

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
 *   constraint_id: ivf_embryo_selection_regulation
 *   human_readable: IVF Embryo Selection Regulation: Coordination vs. Eugenic Extraction
 *   domain: bioethics/reproductive_medicine/regulatory_governance
 *
 * SUMMARY:
 *   IVF embryo selection regulation operates as a hybrid
 *   coordination-extraction mechanism spanning reproductive medicine, genetic
 *   technology markets, disability identity, and regulatory governance. The
 *   constraint manifests as a regulatory framework that simultaneously
 *   enables disease prevention (genuine coordination function) and enables
 *   genetic stratification of the human population (extraction mechanism).
 *   The coordination function is real: genetic screening can prevent serious
 *   monogenic disorders and reduce miscarriage risk, providing genuine
 *   medical benefit to prospective parents and improving health outcomes. But
 *   the extraction mechanism is equally real: the regulatory regime is
 *   captured by the fertility and genetic testing industries, economic
 *   gatekeeping restricts access to the affluent, and the framework
 *   systematically defines genetic diversity (particularly disability) as
 *   acceptable only when selected out. The constraint exhibits the full
 *   spectrum of DR classifications depending on structural position: disabled
 *   persons trapped in an existential snare, prospective parents facing mixed
 *   coordination and coercion, fertility clinics experiencing pure
 *   coordination benefit, genetic testing providers extracting through market
 *   dominance, disability advocates organizing a counter-narrative, bioethics
 *   committees performing degraded oversight, and analytical observers
 *   risking naturalization of a constructed extractive regime. The theater
 *   ratio (0.64) reflects that bioethics governance and clinical practice
 *   standards are substantially performative — extensive deliberation and
 *   published guidelines mask market-driven expansion of selection criteria
 *   and regulatory capture. The extractiveness trajectory (0.28 → 0.58 over
 *   the interval) shows the constraint degrading from early medical
 *   innovation toward systematic eugenic infrastructure, with the
 *   disease-trait boundary collapsing and selection expanding from serious
 *   monogenic conditions toward polygenic traits (height, intelligence,
 *   behavioral predisposition).
 *
 * KEY AGENTS:
 *   - Fertility clinics: Primary beneficiary (institutional/arbitrage) — capture market value, regulatory legitimacy, premium pricing through genetic selection services
 *   - Genetic testing providers: Secondary beneficiary (powerful/arbitrage) — extract through data collection, patent control, market concentration, and normalization of mandatory screening
 *   - Wealthy parents: Secondary beneficiary (powerful/arbitrage) — access genetic selection, genetic advantage, reproductive autonomy (subject to regulatory constraints)
 *   - Disabled persons: Primary victim (powerless/trapped) — existentially defined as selectable-out; caught in intergenerational framing that genetic diversity should not exist
 *   - Reproductive autonomy commons: Abstract victim (powerless/trapped) — the shared epistemic and social good of reproductive diversity is contaminated by normalization of genetic selection
 *   - Disability rights advocates: Organized counter-actor (organized/constrained) — building alternative regulatory narratives, political organizing, cultural work toward sunset
 *   - Bioethics governance bodies: Institutional actors (institutional/constrained) — nominally oversee but functionally degraded through industry capture and lack of enforcement power
 *   - Prospective parents (middle-income): Mixed actor (moderate/constrained) — benefit from disease prevention coordination but face economic gatekeeping and coercive normalization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ivf_embryo_selection_regulation, 0.58).
domain_priors:suppression_score(ivf_embryo_selection_regulation, 0.68).
domain_priors:theater_ratio(ivf_embryo_selection_regulation, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ivf_embryo_selection_regulation, extractiveness, 0.58).
narrative_ontology:constraint_metric(ivf_embryo_selection_regulation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ivf_embryo_selection_regulation, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ivf_embryo_selection_regulation, tangled_rope).
narrative_ontology:human_readable(ivf_embryo_selection_regulation, "IVF Embryo Selection Regulation: Coordination vs. Eugenic Extraction").
narrative_ontology:topic_domain(ivf_embryo_selection_regulation, "bioethics/reproductive_medicine/regulatory_governance").

domain_priors:requires_active_enforcement(ivf_embryo_selection_regulation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ivf_embryo_selection_regulation, fertility_clinics).
narrative_ontology:constraint_beneficiary(ivf_embryo_selection_regulation, genetic_testing_providers).
narrative_ontology:constraint_beneficiary(ivf_embryo_selection_regulation, wealthy_parents).
narrative_ontology:constraint_beneficiary(ivf_embryo_selection_regulation, reproductive_medicine_industry).
narrative_ontology:constraint_victim(ivf_embryo_selection_regulation, disabled_persons).
narrative_ontology:constraint_victim(ivf_embryo_selection_regulation, genetic_underclass).
narrative_ontology:constraint_victim(ivf_embryo_selection_regulation, reproductive_autonomy_commons).
narrative_ontology:constraint_victim(ivf_embryo_selection_regulation, public_health_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISABLED PERSONS (SNARE) — Trapped by regulatory regimes that define their genetic profiles as selectable-out. The constraint operates intergenerationally: embryos with markers for deafness, dwarfism, autism, or other conditions are systematically culled in regulated regimes. The disabled person cannot exit this framework — their very existence is defined as an outcome to be avoided. Maximum suppression: social narrative frames genetic traits as defects, regulatory regimes enable selection-against, and economic pressure (IVF cost) creates quasi-mandatory selection for 'normal' traits. The extraction is existential: the constraint redefines disability as something that should not exist rather than something that requires accommodation and social inclusion. No beneficiary relationship.
constraint_indexing:constraint_classification(ivf_embryo_selection_regulation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PROSPECTIVE PARENTS (TANGLED ROPE) — Face both coordination and extraction. Genuine coordination function: IVF selection enables disease prevention (BRCA1/2, cystic fibrosis, sickle cell) and informed reproductive choice. But asymmetric extraction occurs through cost gatekeeping (access stratified by wealth), regulatory framing (genetic selection presented as medical necessity rather than parental preference), and coercive normalization (social pressure to 'be a responsible parent' and select for health). Parents with resources experience access; parents without resources experience prohibition or coercive delay. Significant suppression: regulatory regimes make selection mandatory (you cannot use IVF in many jurisdictions without accepting genetic screening), and economic gatekeeping prevents exit. But prospective parents also benefit from the same coordination mechanism — embryo selection can prevent genuine harms.
constraint_indexing:constraint_classification(ivf_embryo_selection_regulation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FERTILITY CLINICS (ROPE) — Experience the constraint as pure coordination. Clinics use genetic testing to improve outcomes, demonstrate competence, and differentiate from competitors. The coordination function is genuine: embryo selection reduces the risk of chromosomal abnormalities, improves implantation rates, and reduces miscarriage. Clinics benefit from regulatory frameworks that legitimize genetic selection, increase market demand for advanced services, and justify premium pricing. Suppression is low for clinics — they can exit if desired (return to pre-genetic selection practices), though market pressure makes this unlikely. Effective extraction runs toward clinics. No victims from clinic perspective.
constraint_indexing:constraint_classification(ivf_embryo_selection_regulation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: GENETIC TESTING PROVIDERS (TANGLED ROPE) — Market actors that benefit enormously from regulatory regimes that mandate genetic screening for IVF. The coordination function exists: accurate genetic testing reduces harm from serious monogenic disorders. But extraction is substantial: genetic testing companies capture vast value through data collection (genetic libraries worth billions), patent control over diagnostic panels, regulatory gatekeeping (competitors cannot easily enter markets), and framing (normalization that selection for 'healthy' genes is medically necessary). Genetic test providers have arbitrage options (exit to other genomic markets) and are powerful globally. Suppression for them is minimal. But they extract through market concentration and regulatory capture — genetic testing has become mandatory rather than optional, and prices remain inflated.
constraint_indexing:constraint_classification(ivf_embryo_selection_regulation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DISABILITY RIGHTS ADVOCATES (SCAFFOLD) — Organized agents building counter-regulatory frameworks. The disability coalition sees embryo selection as a temporary extractive regime that will degrade as social attitudes shift toward disability inclusion, as prenatal/preconception testing moves away from selection-toward-termination and toward informed reproductive autonomy, and as new genetic intervention technologies (somatic gene therapy, CRISPR) shift from selection to treatment. Disability advocates have agency (political organizing, regulatory participation, cultural narrative work) and perceive an exit path: move from selection-based medicine toward treatment-based medicine and universal design accommodating genetic diversity. Suppression is high currently (regulatory capture by medical industry, economic pressure), but the coalition sees a sunset: as disability inclusion norms strengthen and alternative interventions develop, the selection regime's extraction mechanism loses force. Theater ratio is currently high for advocates — their counter-narrative is marginalized as 'anti-medicine' — but sunset dynamics show increasing organizational power.
constraint_indexing:constraint_classification(ivf_embryo_selection_regulation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: BIOETHICS COMMITTEES AND REGULATORY BODIES (PITON) — Institutional actors nominally overseeing embryo selection but functionally degraded. The oversight ritual persists: IVF clinics consult ethics committees, regulatory frameworks declare guidelines, international bodies produce declarations (UNESCO, WHO). But the functional verification capacity is minimal — bioethics lacks enforcement power, clinical practice operates primarily through market logic and professional norms rather than ethics, and the framing of 'selection' as 'screening' obscures the eugenic trajectory. Theater is substantial: ethics committees review protocols but have limited ability to prevent practices, regulatory bodies publish guidelines but cannot compel compliance across jurisdictions, and the language of 'health' naturalizes what is fundamentally a choice about which genetic traits are acceptable. The bioethics system maintains legitimacy through performative deliberation (extensive meetings, published statements) while the extraction mechanism (regulatory capture by fertility industry, market-driven expansion of selection criteria) continues unimpeded.
constraint_indexing:constraint_classification(ivf_embryo_selection_regulation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE MOUNTAIN) — From a civilizational perspective, reproductive selection risks to appear as an immutable feature of human choice: parents have always preferred children without serious disabilities, genetic testing simply makes old preferences more efficient, and eugenic thinking is inherent to human reproduction. This perspective naturalizes selection as inevitable and treats objections as romantic resistance to progress. However, the structural data contradicts the mountain classification: the constraint's existence depends on regulatory regimes, market structures, and framing choices — all contingent institutional arrangements. The extraction is not natural but constructed through policy, pricing, and narrative naturalization. The engine will compute this as a false summit, revealing that 'inherent human nature' is the cover story for what is actually regulatory capture by the fertility industry.
constraint_indexing:constraint_classification(ivf_embryo_selection_regulation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ivf_embryo_selection_regulation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ivf_embryo_selection_regulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ivf_embryo_selection_regulation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ivf_embryo_selection_regulation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ivf_embryo_selection_regulation, TR),
    TR >= 0.70.

:- end_tests(ivf_embryo_selection_regulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Elevated and rising. At time 0 (early IVF era), genetic selection was rare and limited to severe conditions, so base extraction was low (0.28) — the coordination benefit dominated and extraction was incidental. But the measurement trajectory shows consistent increase as selection criteria expand, regulatory frameworks normalize genetic testing, and market dynamics drive expansion toward polygenic trait selection. Current extractiveness reflects the substantial asymmetries: genetic advantage concentration among the wealthy, regulatory framing that makes selection mandatory rather than optional, and eugenic logic expanding from disease prevention toward optimization. Suppression (0.68): High and structural. Multiple layers: (1) economic gatekeeping — IVF genetic selection costs $10,000-20,000+ and is inaccessible to lower-income families; (2) regulatory mandates — in many regimes, genetic screening is mandatory for IVF rather than optional, removing genuine choice; (3) social coercion — prospective parents face peer and provider pressure to select, framed as 'responsible parenting'; (4) epistemic suppression — alternative framings (disability inclusion, genetic diversity as feature not bug) are marginalized or pathologized as 'anti-medicine'; (5) disability erasure — the existence and experiences of disabled persons are treated as unfortunate outcomes to be prevented rather than respected communities to include. Theater ratio (0.64): Substantial and increasing. Bioethics governance is performative: ethics committees review protocols but lack enforcement power; regulatory bodies issue guidelines repeatedly violated by clinical practice; the language shifts from 'selection' to 'screening' to disguise choice as medical necessity; 'health' framing naturalizes what is fundamentally a eugenic sorting mechanism. The theater serves to legitimize the extraction by framing it as medical science rather than social choice. Trajectory shows theater increasing as the constraint expands — more elaborate ethical language is needed to justify increasingly aggressive selection criteria.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the beneficiary (institutional/arbitrage — rope) and victim (powerless/trapped — snare) perspectives is the core diagnostic signal. The same constraint appears as pure coordination to those who benefit and pure extraction to those who bear costs. This gap reveals that the 'coordination function' is real but fundamentally asymmetric: genetic screening coordinates disease prevention for those with access and resources, but for those lacking access it is pure constraint. The prospective parent perspective (moderate/constrained — tangled rope) sits between these extremes, experiencing both genuine benefit (disease prevention) and genuine extraction (economic gatekeeping, normalization coercion). The bioethics perspective (piton) reveals performative governance — the oversight apparatus is extensively elaborated but functionally degraded, maintaining legitimacy through ritual rather than actual constraint on practice. The disability advocate perspective (scaffold) is diagnostic: they perceive a sunset because they see alternative regulatory frames and technological paths becoming available. If the scaffold sunset is real, the constraint's extraction mechanism loses force as social alternatives strengthen. If the sunset is aspirational rather than structural, the piton governance system will persist indefinitely.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) reflects each agent's structural relationship to the extraction flow. Fertility clinics are beneficiaries with arbitrage options (can exit to other services) — derived d ≈ 0.12, producing low effective extraction f(d) ≈ 0.01. They capture value but can leave if desired. Genetic testing providers are beneficiaries with powerful positions — derived d ≈ 0.08, f(d) ≈ -0.10 — extraction runs heavily toward them. Disabled persons are victims trapped by the regulatory frame — derived d ≈ 0.98, f(d) ≈ 1.41 — maximum experienced extraction from a powerless position with no exit. Prospective parents are mixed: beneficiaries of disease prevention but victims of economic gatekeeping and normalization coercion — derived d ≈ 0.52, f(d) ≈ 0.65 — moderate effective extraction reflecting mixed benefit and harm. Disability advocates are organized agents perceiving a sunset — derived d ≈ 0.45, f(d) ≈ 0.40 — moderate extraction reflecting constrained current position but perceived agency to change the regime. The directionality computation reveals why the constraint persists: those who benefit (clinics, testing providers, some prospective parents) have high f(d) working in their favor (low d produces negative or low χ for them), while those who bear costs (disabled persons) have very high d with no corresponding organization or exit. The disabled-as-powerless-trapped combination produces maximum f(d) = 1.41, concentrating experienced extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is NOT resolved at its current extractiveness (0.58). The mandatrophy question is: does genetic selection genuinely coordinate disease prevention, or is 'disease prevention' a cover story for eugenic extraction? The data shows BOTH are occurring simultaneously, which is exactly when tangled rope classification applies. Genuine coordination: serious monogenic disorders (cystic fibrosis, BRCA1/2, sickle cell) have high morbidity and mortality; embryo selection can prevent these conditions with high efficacy; prospective parents genuinely benefit from avoiding serious disease outcomes. The coordination function is real. But equally real extraction: genetic testing is becoming normalized and mandatory rather than optional; selection criteria are expanding beyond serious disease toward polygenic traits and optimization; the economic structure creates a genetic underclass (those who cannot afford genetic selection lose competitive advantage); the regulatory capture is near-total (industry-funded research, revolving-door governance, suppression of alternative frameworks); and the existential impact on disabled persons is severe (genetic diversity is framed as acceptable only if selected out). The constraint cannot be resolved by claiming that genetic selection is 'only medicine' (it is also eugenic) or 'only choice' (it is also coercive). The resolution requires: (1) tightening the disease-trait boundary and enforcing it through regulation; (2) expanding disability inclusion policies so that genetic diversity does not require eugenic elimination; (3) democratizing access to genetic technology so that genetic advantage is not concentration by wealth; (4) developing somatic interventions (gene therapy) as alternatives to selection-based medicine; (5) breaking industry capture of bioethics governance through independent funding and enforcement power. Until these conditions are met, the tangled rope classification stands and the constraint persists as both coordination and extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    disease_vs_trait_boundary,
    'What is the boundary between selecting against serious disease (monogenic Mendelian conditions) versus selecting for traits (intelligence, appearance, behavioral predisposition)?',
    'Historical analysis of medical necessity standards; clinical outcomes data comparing disease vs trait selection; international regulatory policy comparison showing where boundary is drawn in different jurisdictions',
    'If boundary is firm and enforced: constraint remains a tangled rope coordinating disease prevention with mild extraction. If boundary collapses and trait selection becomes routine: constraint degrades into snare (pure eugenic extraction). Currently the boundary is shifting — BRCA selection for cancer risk now accepted; height selection emerging; intelligence-related screening under development.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disease_vs_trait_boundary, empirical, 'Whether disease-trait boundary remains stable or collapses into unrestricted selection').

omega_variable(
    disability_social_model_adoption,
    'Do societies with strong disability rights frameworks and universal design policies show measurably lower rates of genetic selection for conditions like deafness, autism, and dwarfism?',
    'Comparative analysis: IVF genetic selection rates for the same conditions across countries with different disability inclusion policies (Nordic model vs. Anglo-American vs. East Asian); longitudinal tracking as disability policies change; qualitative data from prospective parents on decision-making in different policy contexts',
    'If strong correlation: disability inclusion policies can mitigate the eugenics trajectory of embryo selection, supporting the scaffold perspective and sunset hypothesis. If weak correlation: genetic selection pressure is driven by parental preference independent of policy, and the extractive framework is more entrenched than appears.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disability_social_model_adoption, empirical, 'Whether disability inclusion policies reduce genetic selection rates').

omega_variable(
    somatic_intervention_displacement,
    'As somatic gene therapy, CRISPR-based treatments, and pharmaceutical interventions for genetic conditions mature, do they displace embryo selection as the primary prevention pathway?',
    'Clinical outcomes data comparing embryo selection vs. postnatal somatic intervention for the same conditions; cost-effectiveness analysis; regulatory approval trends for gene therapy; prospective parent preference studies as somatic options mature',
    'If somatic interventions displace selection: the scaff sunset is structural — as technology options change, selection becomes optional rather than mandatory. If selection persists despite somatic options available: the constraint is driven by parental preference for genetic control rather than disease prevention, revealing deeper eugenic logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(somatic_intervention_displacement, empirical, 'Whether somatic gene therapy displaces embryo selection as primary prevention method').

omega_variable(
    regulatory_capture_intensity,
    'How thoroughly has the genetic testing industry captured bioethics governance bodies, clinical practice guidelines, and regulatory standard-setting?',
    'Institutional analysis: funding flows to bioethics organizations from fertility and genetics industry; revolving door analysis (how many guidelines authors have financial ties to testing providers); analysis of conflict-of-interest disclosures; comparative examination of pre- vs. post-industry-involvement regulatory positions',
    'If capture is near-total: the piton classification is too generous — the bioethics system is completely hollowed out and should be reclassified as snare (pure extraction under theater). If capture is partial: the tangled rope classification holds and counter-organizing (disability coalition) remains viable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture_intensity, empirical, 'Degree of regulatory capture by genetic testing industry').

omega_variable(
    reproductive_coercion_mechanism,
    'Is economic gatekeeping (IVF cost, insurance coverage decisions) or social normalization (peer pressure to use genetic selection) the primary coercion mechanism that drives selection adoption?',
    'Survey data from prospective parents: reported reasons for genetic selection adoption; comparison of selection rates across insurance coverage regimes (universal coverage vs. out-of-pocket); qualitative interviews on decision-making process; analysis of clinical practice norms and expectations',
    'If economic: the suppression can be mitigated by public funding and insurance coverage policies. If social normalization: the suppression is harder to address — even parents with resources and alternatives may feel pressure to select. If both: interventions must address both axes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reproductive_coercion_mechanism, empirical, 'Primary coercion mechanism driving genetic selection adoption').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ivf_embryo_selection_regulation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ivf_embryo_tr_t0, ivf_embryo_selection_regulation, theater_ratio, 0, 0.38).
narrative_ontology:measurement(ivf_embryo_tr_t5, ivf_embryo_selection_regulation, theater_ratio, 5, 0.51).
narrative_ontology:measurement(ivf_embryo_tr_t10, ivf_embryo_selection_regulation, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(ivf_embryo_be_t0, ivf_embryo_selection_regulation, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ivf_embryo_be_t5, ivf_embryo_selection_regulation, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(ivf_embryo_be_t10, ivf_embryo_selection_regulation, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ivf_embryo_selection_regulation, resource_allocation).
narrative_ontology:boltzmann_floor_override(ivf_embryo_selection_regulation, 0.22).
narrative_ontology:affects_constraint(ivf_embryo_selection_regulation, reproductive_genetic_advantage_stratification).
narrative_ontology:affects_constraint(ivf_embryo_selection_regulation, disability_identity_and_social_model).
narrative_ontology:affects_constraint(ivf_embryo_selection_regulation, bioethics_regulatory_capture).

% DUAL FORMULATION NOTE:
% IVF embryo selection decomposition: (1) genetic_disease_prevention_coordination (ε≈0.15, Rope) — actual medical function preventing monogenic conditions; (2) genetic_trait_expansion (ε≈0.72, Snare) — expansion toward polygenic traits and optimization, pure extraction; (3) ivf_embryo_selection_regulation (ε≈0.58, Tangled Rope) — the hybrid constraint as currently instantiated. The constraint family shows the regulatory frame collapsing the disease-trait boundary, causing the mechanism to degrade from pure coordination toward pure extraction. IVF embryo selection regulation affects reproductive_genetic_advantage_stratification (downstream wealth-based genetic stratification), disability_identity_and_social_model (the framing of genetic diversity as defect vs. difference), and bioethics_regulatory_capture (the governance system's degradation through industry capture).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ivf_embryo_selection_regulation, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
