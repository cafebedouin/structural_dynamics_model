% ============================================================================
% CONSTRAINT STORY: hominin_evolutionary_bottleneck
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hominin_evolutionary_bottleneck, []).

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
 *   constraint_id: hominin_evolutionary_bottleneck
 *   human_readable: The Hominin Evolutionary Bottleneck & Replacement Event
 *   domain: paleoanthropology/evolutionary_biology
 *
 * SUMMARY:
 *   The history of paleoanthropology is constrained by a structural
 *   bottleneck: the fossil record of human ancestors is sparse,
 *   geographically uneven, and chronologically discontinuous. For over a
 *   century, this bottleneck created an epistemic monopoly — a single
 *   authorized narrative of human origins, enforced through control of museum
 *   specimens, peer review authority, and the prestige of fossil discovery.
 *   The constraint operates on multiple levels: taphonomic (real limits on
 *   fossilization), institutional (gatekeeping of specimen access and
 *   narrative authority), and epistemic (suppression of hypotheses that posit
 *   fossil gaps as *normal* rather than as evidence of species absence). The
 *   same structural phenomenon appears as an immutable law of paleontology
 *   (mountain), a coordination mechanism for scientific work (rope), a mixed
 *   coordination-extraction hybrid (tangled rope), a degraded ritual (piton),
 *   or pure exclusion (snare), depending on the observer's position within
 *   the system. Ancient DNA and paleogenomics now provide an alternative
 *   verification pathway that bypasses fossil-record sparsity entirely,
 *   creating a scaffold perspective with a measurable sunset. The
 *   constraint's theater_ratio (0.68) reflects that taxonomic naming,
 *   phylogenetic trees, and fossil 'completeness' narratives are
 *   substantially performative — they provide the appearance of definitive
 *   knowledge where deep uncertainty remains.
 *
 * KEY AGENTS:
 *   - Incomplete Fossil Record: Primary victim (powerless/trapped) — the record itself cannot testify to its own incompleteness; gaps are interpreted as absence of species
 *   - Alternative Evolutionary Hypotheses: Secondary victim (powerless/trapped) — hypotheses predicting normal fossil gaps are structurally suppressed by orthodox interpretation
 *   - Indigenous Knowledge Systems: Secondary victim (powerless/trapped) — oral histories and place-based knowledge systematically excluded from authoritative narratives
 *   - Museum Custodians: Primary beneficiary (institutional/arbitrage) — gatekeeping of specimens reinforces their institutional authority and funding access
 *   - Evolutionary Biology Consensus Authorities: Primary beneficiary (powerful/mobile) — control narrative, prestige, media authority; could exit but benefit more from maintaining system
 *   - Paleoanthropologists in Field: Mixed (moderate/constrained) — benefit from fossil discovery prestige but trapped by the same sparse record limiting hypothesis testing
 *   - Ancient DNA & Genomics Researchers: Organized agents (organized/mobile) — building alternative verification pathway with genuine sunset logic
 *   - Linnean Taxonomy System: Institutional piton (institutional/arbitrage) — performative naming persists through inertia despite weak link to actual evolutionary relationships
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as taphonomic inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hominin_evolutionary_bottleneck, 0.38).
domain_priors:suppression_score(hominin_evolutionary_bottleneck, 0.62).
domain_priors:theater_ratio(hominin_evolutionary_bottleneck, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hominin_evolutionary_bottleneck, extractiveness, 0.38).
narrative_ontology:constraint_metric(hominin_evolutionary_bottleneck, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(hominin_evolutionary_bottleneck, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hominin_evolutionary_bottleneck, tangled_rope).
narrative_ontology:human_readable(hominin_evolutionary_bottleneck, "The Hominin Evolutionary Bottleneck & Replacement Event").
narrative_ontology:topic_domain(hominin_evolutionary_bottleneck, "paleoanthropology/evolutionary_biology").

domain_priors:requires_active_enforcement(hominin_evolutionary_bottleneck).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hominin_evolutionary_bottleneck, fossil_record_custodians).
narrative_ontology:constraint_beneficiary(hominin_evolutionary_bottleneck, evolutionary_narrative_authorities).
narrative_ontology:constraint_victim(hominin_evolutionary_bottleneck, alternative_evolutionary_hypotheses).
narrative_ontology:constraint_victim(hominin_evolutionary_bottleneck, missing_specimen_gaps).
narrative_ontology:constraint_victim(hominin_evolutionary_bottleneck, indigenous_knowledge_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INCOMPLETE FOSSIL RECORD (SNARE) — The fossil record itself is the victim: vast evolutionary transitions remain unattested, leaving gaping intervals in the hominin lineage. No exit from taphonomic constraints (fossilization requires rare conditions; most organisms decompose completely). The sparse record cannot argue its own incompleteness; gaps are interpreted as absence of species, not as absences in the record. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.65.
constraint_indexing:constraint_classification(hominin_evolutionary_bottleneck, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALTERNATIVE EVOLUTIONARY HYPOTHESES (SNARE) — Hypotheses incompatible with the sparse-record consensus (gradualism, mosaic evolution from multiple lineages, rapid local differentiation) cannot accumulate evidence because they predict fossil gaps as *normal*, while the establishment interprets gaps as absence-of-species. These alternatives are structurally suppressed by the orthodox reading of silence. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.64.
constraint_indexing:constraint_classification(hominin_evolutionary_bottleneck, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: INDIGENOUS KNOWLEDGE SYSTEMS (SNARE) — Oral histories, place-based genealogies, and archaeological knowledge embedded in indigenous communities are systematically excluded from the authoritative narrative of human origins. No access to the institutions (museums, universities, peer-reviewed journals) that control the legitimacy of origin claims. Trapped by language barriers, colonialism's epistemic hierarchy, and the refusal to integrate non-Western sources. d≈0.98, f(d)≈1.50, σ=0.8 → χ≈0.59.
constraint_indexing:constraint_classification(hominin_evolutionary_bottleneck, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 4: PALEOANTHROPOLOGISTS IN THE FIELD (TANGLED ROPE) — Individual researchers benefit from the sparse-record paradigm (discoverers gain career prestige, funding flows to excavation expeditions, publications are guaranteed if a fossil is novel) BUT are constrained by the same sparse record that limits their ability to test hypotheses or refute alternatives. They both enforce and are trapped by the bottleneck. d≈0.58, f(d)≈0.78, σ=1.0 → χ≈0.30.
constraint_indexing:constraint_classification(hominin_evolutionary_bottleneck, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: MUSEUM AND ARCHIVE CUSTODIANS (ROPE) — Natural history museums (British Museum, American Museum of Natural History, National Museums of Kenya) benefit from their role as custodians of the canonical fossil record. They coordinate scientific work through specimen access, conservation, and institutional authority. The bottleneck reinforces their gatekeeping function. They experience the constraint as coordination: governing access, curating exhibits, maintaining scientific standards. d≈0.10, f(d)≈-0.05, σ=1.2 → χ≈-0.02.
constraint_indexing:constraint_classification(hominin_evolutionary_bottleneck, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: THE LINNEAN TAXONOMIC SYSTEM (PITON) — Species designation for extinct hominins is largely performative. Taxonomists assign binomial names to isolated or partial skeletons with minimal comparative material, creating an illusion of categorical certainty where deep uncertainty remains (Homo floresiensis, Homo naledi, Homo denisova identity status). The taxonomy persists through institutional inertia — it provides labels that feel scientific — but its functional link to actual evolutionary relationships is tenuous. theater_ratio=0.68 satisfies the piton gate. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.03.
constraint_indexing:constraint_classification(hominin_evolutionary_bottleneck, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANCIENT DNA & GENOMIC REVOLUTION (SCAFFOLD) — aDNA sequencing and paleogenomics (Neanderthal genome, Denisova genome, environmental DNA from sediments) provide an alternative verification pathway for evolutionary history that bypasses the fossil-record bottleneck. This is temporary support with a real sunset: genomic data is already reshaping what we know about hominin diversity, population structure, and admixture. By mid-century, genomic+isotopic+morphometric data may render fossil-rarity a non-constraint. χ≈0.25. d≈0.35, f(d)≈0.28, σ=1.2 → χ≈0.10.
constraint_indexing:constraint_classification(hominin_evolutionary_bottleneck, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: EVOLUTIONARY BIOLOGY CONSENSUS AUTHORITIES (TANGLED ROPE) — Leading figures in paleoanthropology (e.g., Richard Leakey lineage, major museum directors, journal editors) benefit from controlling the narrative of human origins (funding access, prestige, media authority to declare 'settled science'). They also enforce the bottleneck through peer review, field access control, and publication gatekeeping. Powerful + mobile means they could exit, but they benefit more from maintaining the current system. d≈0.35, f(d)≈0.28, σ=1.2 → χ≈0.43.
constraint_indexing:constraint_classification(hominin_evolutionary_bottleneck, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / TAPHONOMIC REALITY (MOUNTAIN) — From a deep-time, universal perspective, the sparse hominin fossil record is fundamentally constrained by taphonomy: fossilization is a rare process requiring specific geological conditions (rapid burial, anoxic environments, mineral-rich sediments). No society, no research program, no innovation can overcome the law that most organisms decompose without fossilizing. This perspective sees the bottleneck as an immutable property of deep time itself. However: extractiveness=0.38, suppression=0.62 contradict a mountain classification (would require ε≤0.25, suppression≤0.05). The engine flags this as a false summit, revealing that social/institutional constraints are being conflated with natural law.
constraint_indexing:constraint_classification(hominin_evolutionary_bottleneck, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hominin_evolutionary_bottleneck_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hominin_evolutionary_bottleneck, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hominin_evolutionary_bottleneck, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(hominin_evolutionary_bottleneck, TR),
    TR >= 0.70.

:- end_tests(hominin_evolutionary_bottleneck_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The bottleneck extracts value from alternative explanations and indigenous knowledge, allowing fossil-custodians to control the narrative. But extraction is not maximal (ε≥0.50) because fossil evidence does constrain the space of viable hypotheses — the consensus is not pure fiction, it is partially evidence-based. Suppression (0.62): Moderately high. Alternative hypotheses face systematic obstacles (peer review bias, funding concentration toward canonical sites, language barriers excluding non-English work), but suppression is not total — some alternative work does get published, and new discoveries regularly force paradigm revisions. Theater_ratio (0.68): High, indicating substantial performativity. Taxonomic naming of fossil species conveys false certainty (Homo naledi: is it a tool-maker? a dead-end branch? contemporary with sapiens? still contested). Phylogenetic trees are drawn as if resolved, while many nodes rest on single or partial specimens. The theater increased over the interval as fossil pressure mounted to show 'solutions' to increasingly obvious gaps (multiregional evolution vs out-of-Africa, Homo floresiensis, new erectus claims).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The incomplete fossil record (powerless, trapped) sees pure extraction (snare) — unable to testify, gaps are weaponized against alternative explanations. Alternative hypotheses (powerless, trapped) see suppression (snare) — their predictive power is dismissed a priori. Indigenous knowledge (powerless, trapped) sees colonial exclusion (snare) — no pathway to institutional legitimacy. Field paleoanthropologists (moderate, constrained) see mixed coordination-extraction (tangled rope) — their discoveries earn prestige but they cannot escape the sparse-record bottleneck. Museum custodians (institutional, arbitrage) see pure coordination (rope) — specimen curation is a genuine epistemic function. Consensus authorities (powerful, mobile) see mixed dynamics with net benefit (tangled rope) — they enforce the bottleneck but could exit if threatened. Taxonomists (institutional) see their own work as performative (piton) — species names are useful labels but their evolutionary significance is often unclear. Genomicists (organized, mobile) see the bottleneck as a temporary coordination problem with a sunset (scaffold) — aDNA already bypasses fossil dependence. The civilizational analyst (analytical) risks seeing taphonomic inevitability (mountain), but the structural data reveals contingent institutional arrangements.
 *
 * DIRECTIONALITY LOGIC:
 *   Incomplete fossil record: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction. Alternative hypotheses: Victim + trapped → d≈0.92, f(d)≈1.38. Severe suppression. Indigenous knowledge: Victim + trapped → d≈0.98, f(d)≈1.50. Maximum extraction + epistemic colonialism. Field paleoanthropologists: Mixed beneficiary/victim + constrained → d≈0.58, f(d)≈0.78. Moderate extraction; they benefit from prestige but are also constrained. Museum custodians: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.05. Net beneficiary; gatekeeping is their institutional function. Consensus authorities: Beneficiary + mobile → d≈0.35, f(d)≈0.28. They have options but benefit more from status quo. Genomicists: Organized agents + mobile → d≈0.35, f(d)≈0.28. Low effective extraction because they have agency and an exit path (alternative methodology). Taxonomists: Institutional + arbitrage → d≈0.05, f(d)≈-0.12. Piton classification comes from theater gate, not high chi.
 *
 * MANDATROPHY ANALYSIS:
 *   CLASSIFICATION RESOLUTION: This constraint defies simple mandatrophy because it is genuinely a Tangled Rope. The fossil-record bottleneck is NOT pure extraction (snare) — the sparse record does provide real empirical constraints on evolutionary hypotheses, and fossil discoveries do advance our understanding. Nor is it pure coordination (rope) — suppression of alternatives and institutional gatekeeping create extractive dynamics. The constraint is a hybrid: it solves a genuine coordination problem (how do we know human evolutionary history?) while simultaneously extracting value from those excluded (alternative hypotheses, indigenous knowledge). The theater_ratio (0.68) reveals degradation of the coordination function — much of what looks like science is actually performance (naming, tree-drawing, narrative certainty) — but the function is not yet fully piton (the constraint retains real empirical power). The scaffold perspective (ancient DNA) is real and expanding; within 20-40 years, genomic data will likely make fossil-record sparsity irrelevant to most evolutionary questions. At that point, the constraint may degrade from tangled rope to piton (museum curation becomes mainly theatrical) or fully sunset. The mandatrophy is resolved by accepting that institutional arrangements can genuinely mix coordination and extraction; the deferential question is whether the extraction exceeds what coordination requires.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    taphonomic_inevitability_vs_institutional,
    'Is the sparse hominin fossil record primarily a consequence of taphonomic law (inevitable rarity of fossilization) or an institutional artifact (suppression of alternative narratives, inaccessible specimens in private collections, bias toward Western-led excavations)?',
    'Comparative analysis: cross-validate documented fossils vs estimated population sizes & geographic distribution over time; inventory all known specimens globally including those in non-Western museums and collections; statistical modeling of expected vs observed fossils under different taphonomic regimes',
    'If taphonomic: the sparse record is a mountain (ε≤0.15), and alternative hypotheses fail on empirical grounds. If institutional: the sparse record is a snare (ε≥0.50), and suppression of alternatives is a deliberate extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taphonomic_inevitability_vs_institutional, empirical, 'Taphonomic inevitability versus institutional suppression').

omega_variable(
    specimen_accessibility_bias,
    'Do significant hominin specimens exist in non-Western, non-English-language institutions or private African collections that are underrepresented in the English-language paleoanthropological literature?',
    'Comprehensive catalog of all known hominin fossils by repository location; analysis of citation patterns in major journals (Nature, PNAS, Journal of Human Evolution) to quantify representation of specimens held outside Western museums; interviews with paleoanthropologists in Africa, Asia, and the Pacific regarding specimen access barriers',
    'If significant underrepresentation found: institutional bottleneck is real, and the sparse-record consensus is partially an artifact of epistemic colonialism. If well-represented: the record is as complete as accessible data suggests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(specimen_accessibility_bias, empirical, 'Specimen accessibility bias in institutional repositories').

omega_variable(
    alternative_hypothesis_exclusion_mechanism,
    'Are hypotheses predicting fossil gaps as *normal* (e.g., mosaic evolution, rapid local speciation, population structure models) systematically rejected by peer review, or do they fail on genuine empirical grounds?',
    'Content analysis of peer review comments on papers proposing alternative phylogenetic models; analysis of publication bias (track acceptance rates for papers proposing sparse-record consensus vs alternatives); modeling of alternative hypotheses under explicit taphonomic assumptions to test empirical falsifiability',
    'If systematic rejection: the bottleneck enforces a narrative monopoly (snare behavior). If fair empirical failure: the consensus is evidence-based.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_hypothesis_exclusion_mechanism, empirical, 'Systematic exclusion of alternative evolutionary hypotheses').

omega_variable(
    ancient_dna_displacement_timeline,
    'Over what timeframe will ancient DNA, paleogenomics, and isotopic/morphometric analysis make fossil-record sparsity irrelevant to evolutionary reconstruction?',
    'Longitudinal study of how aDNA findings have already displaced or reframed fossil-only inferences (e.g., Neanderthal introgression, Denisova population structure); projection of specimen recovery rates and sequencing costs; survey of paleoanthropologists on expected evidentiary transitions',
    'If displacement occurs within 20 years: scaffold sunset is real, tangled_rope → scaffold shift is measurable. If much longer: scaffold perspective is aspirational, constraint persists as snare/piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ancient_dna_displacement_timeline, empirical, 'Timeline for ancient DNA displacement of fossil-record sparsity').

omega_variable(
    indigenous_knowledge_integration_feasibility,
    'Can indigenous oral histories, place-based genealogies, and archaeological knowledge be systematically integrated with biomolecular and fossil evidence to produce a richer evolutionary narrative, or are they incommensurable epistemically?',
    'Case studies of successful indigenous-scientist collaborations (e.g., Aboriginal Australian archaeologists, First Nations paleontologists); linguistic analysis of genealogical depth in oral traditions; cross-validation of indigenous place names with archaeological site distributions',
    'If integrable: indigenous knowledge systems move from snare (trapped, excluded) to rope/tangled_rope (coordinated knowledge production). If incommensurable: the exclusion reflects genuine epistemic boundaries, not just power asymmetry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigenous_knowledge_integration_feasibility, conceptual, 'Feasibility of integrating indigenous knowledge into evolutionary narrative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hominin_evolutionary_bottleneck, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hominin_tr_t0, hominin_evolutionary_bottleneck, theater_ratio, 0, 0.42).
narrative_ontology:measurement(hominin_tr_t50, hominin_evolutionary_bottleneck, theater_ratio, 50, 0.58).
narrative_ontology:measurement(hominin_tr_t100, hominin_evolutionary_bottleneck, theater_ratio, 100, 0.68).

% Extraction over time
narrative_ontology:measurement(hominin_be_t0, hominin_evolutionary_bottleneck, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(hominin_be_t50, hominin_evolutionary_bottleneck, base_extractiveness, 50, 0.33).
narrative_ontology:measurement(hominin_be_t100, hominin_evolutionary_bottleneck, base_extractiveness, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hominin_evolutionary_bottleneck, information_standard).
narrative_ontology:affects_constraint(hominin_evolutionary_bottleneck, human_origins_narrative_authority).
narrative_ontology:affects_constraint(hominin_evolutionary_bottleneck, paleogenomics_verification_displacement).

% DUAL FORMULATION NOTE:
% The hominin evolutionary bottleneck decomposes into two structurally distinct constraints: (1) taphonomic_fossilization_limit (ε≈0.12, Mountain) — the natural law that most organisms decompose without fossilizing; (2) institutional_fossil_monopoly (ε≈0.38, Tangled Rope) — the social enforcement of a single authoritative narrative. They are linked via network.affects_constraints because the institutional constraint is justified by appeal to the taphonomic constraint (claimed inevitability launders actual power asymmetry). The fossil monopoly depends on the false identification of institutional bottleneck with natural law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hominin_evolutionary_bottleneck, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
