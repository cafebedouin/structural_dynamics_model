% ============================================================================
% CONSTRAINT STORY: two_domain_ancestry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_two_domain_ancestry, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: two_domain_ancestry
 *   human_readable: The Two-Domain Tree of Life
 *   domain: genomics/evolutionary_biology/scientific_classification
 *
 * SUMMARY:
 *   The discovery of Asgard archaea and the resulting two-domain model of
 *   life represents a fundamental reclassification of biological ancestry
 *   that dissolves the three-domain taxonomy that dominated molecular biology
 *   since Woese's 1977 proposal. Genomic evidence demonstrates that
 *   eukaryotes (all complex life) evolved within the archaeal domain, not as
 *   a third, coordinate lineage. This constraint embodies the tension between
 *   scientific discovery and institutional frameworks: the phylogenetic fact
 *   (eukaryotes are archaea) conflicts with decades of pedagogical,
 *   institutional, and linguistic commitment to three separate domains. The
 *   constraint exhibits significant perspectival heterogeneity — from the
 *   traditional taxonomy practitioner's snare (career investment in obsolete
 *   model) to the genomic research community's rope (coordinate discovery
 *   enabling new research) to the institutional legacy's piton (three-domain
 *   language persisting through institutional inertia). Theater ratio has
 *   increased from 0.35 to 0.55 over the interval, reflecting growing gap
 *   between institutional language (still using three-domain terms) and
 *   scientific understanding (two-domain accepted by genomic consensus).
 *   Extractiveness has risen from 0.18 to 0.38 as the constraint has shifted
 *   from a research-level finding to a field-wide reorganization requiring
 *   institutional adaptation.
 *
 * KEY AGENTS:
 *   - Genomic Research Community: Primary beneficiary (institutional/arbitrage) — gains prestige, funding, and research directions from paradigm-shifting discovery; experiences constraint as enabling coordination
 *   - Traditional Taxonomy Practitioners: Primary victim (powerless/trapped) — invested careers in three-domain model; faces institutional pressure to update without agency to stop discovery
 *   - Textbook Publishing Industry: Secondary victim (moderate/constrained) — benefits from pedagogical continuity, constrained by revision cycles; requires active enforcement of new standard
 *   - Curriculum Reform Coalition: Organized agents (organized/constrained) — coordinate transition through educational institutions; see sunset clause (new graduates naturally trained in two-domain model)
 *   - Three-Domain Institutional Legacy: Institutional actor (institutional/arbitrage) — maintains three-domain terminology through museums, reference works, naming conventions; primarily performative persistence
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional reckoning as phylogenetic discovery itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(two_domain_ancestry, 0.38).
domain_priors:suppression_score(two_domain_ancestry, 0.48).
domain_priors:theater_ratio(two_domain_ancestry, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(two_domain_ancestry, extractiveness, 0.38).
narrative_ontology:constraint_metric(two_domain_ancestry, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(two_domain_ancestry, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(two_domain_ancestry, tangled_rope).
narrative_ontology:human_readable(two_domain_ancestry, "The Two-Domain Tree of Life").
narrative_ontology:topic_domain(two_domain_ancestry, "genomics/evolutionary_biology/scientific_classification").

domain_priors:requires_active_enforcement(two_domain_ancestry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(two_domain_ancestry, genomic_research_community).
narrative_ontology:constraint_beneficiary(two_domain_ancestry, asgard_archaeal_discovery_groups).
narrative_ontology:constraint_victim(two_domain_ancestry, three_domain_paradigm_advocates).
narrative_ontology:constraint_victim(two_domain_ancestry, traditional_taxonomy_practitioners).
narrative_ontology:constraint_victim(two_domain_ancestry, textbook_publishing_institutions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRADITIONAL TAXONOMY PRACTITIONER (SNARE) — Invested entire careers in three-domain model (Archaea, Bacteria, Eukarya). Cannot easily exit or reframe pedagogical frameworks. Faces institutional pressure to update course materials, textbooks, and taxonomic classifications while bearing reputational cost of prior commitment. Powerless to stop the constraint; trapped by career investment and institutional inertia. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.63.
constraint_indexing:constraint_classification(two_domain_ancestry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TEXTBOOK PUBLISHING INDUSTRY (TANGLED ROPE) — Benefits from pedagogical continuity and slow adoption cycles (textbooks remain current 5-10 years); simultaneously bears costs of major revisions and market confusion during transition. Has some agency (can choose timing of revisions) but constrained by adoption cycles and institutional lag. Requires active enforcement of new standard through curriculum updates. d≈0.68, f(d)≈1.03, σ=1.0 → χ≈0.40.
constraint_indexing:constraint_classification(two_domain_ancestry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: GENOMIC RESEARCH COMMUNITY (ROPE) — Primary beneficiary. Gains significant research prestige, publication opportunities, and grant funding from paradigm-shifting discovery. Experiences the constraint as enabling coordination: the two-domain model provides clearer phylogenetic framework, resolves longstanding questions about eukaryotic origin, and opens new research directions (Asgard archaeal biology, archaeal-eukaryotic symbiosis). Institutional players (Nature, Cell, major genomics labs) can arbitrage early adoption for priority and citations. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.05.
constraint_indexing:constraint_classification(two_domain_ancestry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CURRICULUM REFORM COALITION (SCAFFOLD) — Organized agents (biology education societies, curriculum standardization bodies, accreditation boards) see the transition as temporary coordination failure with built-in sunset: as new cohorts of students graduate with two-domain training, three-domain trained practitioners naturally age out. Reform has clear endpoint and mechanism. Suppression exists (institutional resistance, textbook lag) but is temporary and declining. d≈0.35, f(d)≈0.35, σ=1.0 → χ≈0.11.
constraint_indexing:constraint_classification(two_domain_ancestry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: THREE-DOMAIN INSTITUTIONAL LEGACY (PITON) — The three-domain model persists through museum exhibits, reference books, educational media, and institutional naming (university departments, research centers retain 'Archaea Department'). Theater ratio is high: institutional structures continue using three-domain language long after scientific consensus shifts to two-domain. The model is maintained through inertia and institutional convenience, not functional necessity. theater_ratio≈0.55 reflects moderate degradation — some genuine pedagogical argument remains (three domains as teaching scaffold), but majority is performative institutional continuity. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.02.
constraint_indexing:constraint_classification(two_domain_ancestry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / PHYLOGENETIC REALITY (MOUNTAIN) — From a civilizational perspective, the two-domain structure is determined by evolutionary history: it is what actually happened (eukaryotes evolved from within Archaea). This constraint appears as a natural law of phylogenetic structure, not a contingent institutional fact. However, the structural data (ε=0.38, suppression=0.48, requires_active_enforcement=true, theater=0.55) contradicts true mountain classification. The engine will detect this as a false summit: phylogenetic reality is being naturalized, but the constraint is actually an institutional reckoning with that reality, not the reality itself.
constraint_indexing:constraint_classification(two_domain_ancestry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(two_domain_ancestry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(two_domain_ancestry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(two_domain_ancestry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(two_domain_ancestry, TR),
    TR >= 0.70.

:- end_tests(two_domain_ancestry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-high. The genomic research community captures significant prestige and research opportunity advantage from being early to the two-domain model. However, the extraction is not severe because: (a) the discovery itself is genuine (eukaryotes actually did evolve from archaea), so the research advantage is partly fair attribution; (b) the discovery quickly becomes open knowledge (published in major journals, freely available), limiting opportunity for sustained rent-seeking. Suppression (0.48): Moderate. Institutional barriers to adoption include textbook lag (5-10 year revision cycles), cultural inertia in pedagogy, institutional naming structures, and resistance from researchers trained in three-domain model. However, suppression is not overwhelming — the evidence is clear, the scientific consensus is building, and alternatives are accessible. Theater ratio (0.55): Moderate. Institutional structures (museums, reference books, university department names) continue using three-domain language despite scientific consensus shift. However, theater is not overwhelming because the pedagogical transition is proceeding relatively quickly in undergraduate curricula; this is not a case of pure performative ritual (like the three-body problem in physics education) but rather a genuine lag in institutional updating.
 *
 * PERSPECTIVAL GAP:
 *   The constraint generates perspectival diversity because institutional commitment (three-domain model) has decoupled from empirical reality (two-domain structure). The genomic research community experiences genuine coordination — they are solving the actual problem of eukaryotic origin and opening new research. Traditional practitioners experience pure extraction — they must abandon career investments without compensation. The textbook industry experiences mixed extraction and coordination — revision costs are real, but adoption cycles also provide market stability. The curriculum reform coalition sees a solvable temporary problem with natural sunset. The institutional legacy sees its own degraded ritual. The analytical observer risks seeing phylogenetic inevitability rather than institutional adjustment. This perspectival richness arises because the constraint is fundamentally about the institutional lag between discovery and application, not about the discovery itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Genomic research community: Beneficiary + institutional arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; low extraction relative to them. Traditional practitioners: Victim + powerless trapped → d≈0.92, f(d)≈1.38. Maximum extraction — they cannot exit and must absorb costs. Textbook industry: Victim + moderate constrained → d≈0.68, f(d)≈1.03. Moderate extraction; they have some agency in revision timing but are constrained by adoption cycles. Curriculum coalition: Organized constrained → d≈0.35, f(d)≈0.35. Low extraction; organized agents see a path forward and natural sunset. Three-domain legacy: Institutional arbitrage → d≈0.05, f(d)≈-0.12. Net beneficiary in theater (institutional convenience of persisting terminology). Analytical observer: analytical → d≈0.70, f(d)≈1.13. Mountain classification would be false summit — naturalizing institutional lag as phylogenetic law.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint operates at the institutional/pedagogical level, not the discovery level. The phylogenetic fact (eukaryotes evolved from archaea) is not the constraint — it is what makes the constraint exist. The constraint is the institutional reckoning with that fact: the gap between scientific understanding and institutional frameworks. This prevents misclassification as a pure natural law (mountain). The constraint cannot be simply 'true' (what would that mean? the discovery cannot be undiscovered, but the institutional transition can stall indefinitely). The two-domain classification reflects genuine benefits (coordination of knowledge, research productivity) and asymmetric costs (traditional practitioners' career obsolescence, textbook revision burden). The scaffold perspective confirms the sunset mechanism: as new cohorts graduate with two-domain training, three-domain practitioners age out. The piton perspective shows the institutional legacy of three-domain language persisting through convenience, not necessity. The snare and rope perspectives capture the asymmetry: who benefits and who bears costs. Resolving mandatrophy also requires recognizing that the 'false mountain' is a real phenomenon: powerful actors and institutions have incentives to naturalize institutional arrangements (calling them 'how science works') rather than recognizing them as contingent social facts. The two-domain constraint makes this visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    asgard_archaeal_monophyly,
    'Do Asgard archaea form a monophyletic clade that includes the last archaeal common ancestor of eukaryotes, or is eukaryotic origin a polyphyletic acquisition from multiple archaeal lineages?',
    'Comparative phylogenomics across more Asgard lineages; identification of conserved protein families linking Asgard to eukaryotic homologs; metagenomic surveys of uncultured Asgard populations',
    'If monophyletic: two-domain model is definitive. If polyphyletic: intermediate models (partial third-domain, fuzzy domain boundaries) may emerge, complicating the pedagogical transition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(asgard_archaeal_monophyly, empirical, 'Whether Asgard archaea form the monophyletic eukaryotic ancestor').

omega_variable(
    functional_eukaryotic_features_in_archaea,
    'Do cultivable Asgard archaea possess the full suite of eukaryotic-characteristic protein folding, vesicular trafficking, and cytoskeletal machinery, or are these features still unique to eukaryotes?',
    'Structural characterization of Asgard proteins; cultivation and biochemical analysis of Asgard cells; comparative proteomics with eukaryotic and bacterial systems',
    'If Asgard has eukaryotic-like machinery: archaeal-eukaryotic boundary dissolves functionally, reinforcing two-domain model. If Asgard lacks these features: eukaryotic origin becomes hybrid (archaeal genomes + bacterial cytoplasm via symbiosis), complicating two-domain claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_eukaryotic_features_in_archaea, empirical, 'Whether Asgard archaea possess eukaryotic-characteristic cellular machinery').

omega_variable(
    horizontal_gene_transfer_saturation,
    'How much of eukaryotic genome complexity comes from lateral gene transfer (LGT) from bacteria vs. vertical inheritance from archaeal ancestor? Does LGT volume suggest bacteria are co-authors of eukaryotic origin?',
    'Phylogenetic mapping of bacterial-origin genes in eukaryotic genomes; temporal reconstruction of LGT events; comparison with models of eukaryotic-archaeal inheritance ratios',
    'If LGT < 10% by gene count: two-domain model holds (minor bacterial contributions). If LGT > 30%: three-way hybrid (archaeal nucleus + bacterial cytoplasm + ongoing bacterial import) may better describe eukaryotic origin, partially reviving three-domain conceptual framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(horizontal_gene_transfer_saturation, empirical, 'Proportion of eukaryotic complexity derived from bacterial vs archaeal sources').

omega_variable(
    domain_concept_utility,
    'Is the domain-level taxonomic rank still functionally useful for describing life''s diversity, or should the two/three-domain distinction be abandoned in favor of continuums or network-based phylogenetic representation?',
    'Educational efficacy studies comparing three-domain vs two-domain vs network-based teaching; empirical analysis of domain boundaries (morphological, genomic, functional); alternative taxonomic framework adoption in research communities',
    'If domains are still useful: two-domain model provides stable pedagogical framework. If domains become obsolete: the constraint dissolves entirely — the category itself (domain-level taxonomy) becomes an artifact, making the transition to two domains a pyrrhic victory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(domain_concept_utility, conceptual, 'Whether domain-level taxonomic rank remains conceptually useful').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(two_domain_ancestry, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tda_tr_t0, two_domain_ancestry, theater_ratio, 0, 0.35).
narrative_ontology:measurement(tda_tr_t7, two_domain_ancestry, theater_ratio, 7, 0.48).
narrative_ontology:measurement(tda_tr_t15, two_domain_ancestry, theater_ratio, 15, 0.55).

% Extraction over time
narrative_ontology:measurement(tda_be_t0, two_domain_ancestry, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(tda_be_t7, two_domain_ancestry, base_extractiveness, 7, 0.28).
narrative_ontology:measurement(tda_be_t15, two_domain_ancestry, base_extractiveness, 15, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(two_domain_ancestry, information_standard).
narrative_ontology:affects_constraint(two_domain_ancestry, eukaryotic_origin_symbiosis).
narrative_ontology:affects_constraint(two_domain_ancestry, archaeal_metabolic_diversity).
narrative_ontology:affects_constraint(two_domain_ancestry, last_eukaryotic_common_ancestor).

% DUAL FORMULATION NOTE:
% Two-Domain Ancestry is decomposed from a larger constraint family on eukaryotic origin. The two-domain model is upstream and foundational; downstream constraints (eukaryotic-archaeal symbiosis details, LECA properties, archaeal metabolic innovations) depend on accepting the two-domain structure. Eukaryotic origin was historically formulated as a three-domain problem; the Asgard discovery shifted it to a two-domain nested hierarchy. Separate stories track (a) the phylogenetic claim (two vs three domains as evolutionary fact, ε≈0.08, approaching mountain as evidence solidifies) and (b) the institutional constraint (pedagogical/linguistic transition lag, ε≈0.38, tangled rope as described here). The institutional constraint story is this JSON; the phylogenetic fact story would have lower ε and higher accessibility_collapse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
