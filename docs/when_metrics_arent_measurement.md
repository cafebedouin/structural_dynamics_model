# When Metrics Aren't Measurement

### Cluster-Space Architecture and the Limits of Signature-Pathway Detection

**cafebedouin@gmail.com**

---

#### Abstract

The Deferential Realism apparatus's primary visible work is observer-site disagreement detection: cohomological obstruction, FCR/FSM/FNL overrides, and the cover-story mechanism are all observer-side operations. Beneath that work, the apparatus computes a 16-dimensional metric vector per constraint and runs a deterministic rule cascade against it. This paper takes the metric layer as its object of study and develops a single architectural claim: the metric layer is not a measurement of structural reality but a routing mechanism that pre-partitions constraints by anomaly region. The actual discriminations the apparatus performs — which axiom fires, which signature is assigned, which detection pathway determines the type — happen one layer up, in the rule cascade. Metric similarity registers that some detector fires within an anomaly region; it does not register which.

A five-phase empirical audit on the corpus develops three evidentiary pillars. First, three cluster spaces (observer, idea, metric) are empirically distinct, but distinctness is the weak finding: observer × metric correlate at ρ = 0.776, while idea space correlates with both at only ρ ≈ 0.20, and idea space's three sub-measures (beneficiary, coupling, semantic) are uncorrelated with each other. The apparatus is observer-complete and idea-incomplete. Second, the metric layer quotients out the rule cascade: declared-beneficiary mountains, beneficiaryless FCR-caught mountains, and FSM-caught mountains all sit at cosine similarity ≈ 0.72 to each other in metric space while sitting at ≈ 0.50 to clean mountains and to genuine FCR tangled ropes. Different detection pathways produce metrically indistinguishable constraints. Third, a topic-domain bisection of the lensing zone (high semantic, low observer-agreement) shows that 98.4% is genuine cross-axiom tension, with the apparatus correctly distinguishing mathematical natural laws from their coordination-mechanism semantic neighbors where embedding similarity conflates them.

A small architectural finding closes the paper: a planted-anchor test for unmarked false summits returned three named misfires on mathematical theorems where FCR fires on perspective-coupling patterns authored into the .pl file rather than on extraction structure in the constraint. The metric layer collapses pathways into anomaly regions; the signature layer collapses pathways into output classes. Neither layer alone preserves detection causality. Apparatus self-knowledge requires reading triggers, not signatures or metrics in isolation. The cluster-space taxonomy and the metric-vs-signature finding together motivate the move forward to idea-site cohomology, taken up in a companion paper.

---

#### 1. View Collapse and the Cluster-Space Taxonomy

A view, in this framework's sense, collapses the possibility space of what a constraint can be into a truth from a position. The trifurcation (*Debugging Philosophy*) classified failures of view-formation: drift across views (Type A), inconsistency within a view (Type B), underspecification of which view (Type C). The DR apparatus implements this taxonomy as observer-indexed presheaf computation: at each of four observer positions, a view collapses into a type, and the apparatus measures how those views fail to glue into a global section.

Each view is a function from structural data to a type assignment, and each function operates in a particular space. Three such spaces are distinguishable in principle:

- *Observer space:* the variation in collapse functions across observer positions — what the apparatus's H¹ measures.
- *Idea space:* the structure of what is being collapsed — relations between constraints (shared beneficiaries, coupling, semantic affinity) — currently uncomputed except as auxiliary annotations.
- *Metric space:* the apparatus's lossy encoding of view outputs — the 16-dimensional metric vector that drives the rule cascade.

The trifurcation classified failures by which collapse function went wrong. This paper classifies them by which space the failure lives in, and the difference yields a structural claim about the apparatus the trifurcation does not surface.

The claim: the apparatus's metric layer is not a measurement of structural reality but a routing mechanism that pre-partitions constraints by anomaly region. The actual discriminations the apparatus performs happen one layer up, in the rule cascade. The metric vector tells the cascade which inputs to evaluate; the cascade tells the rest of the apparatus which signature to assign. Metric similarity between two constraints does not register which detector fired on either of them — only that some detector fired on both within the same anomaly region.

This is not a critique of the apparatus's design. It is a clarification of what the apparatus does. The metric layer is correctly described, in its foundational document (*metrics_as_routing*), as a routing mechanism rather than a truth claim. This paper develops the empirical consequence of that description: metric similarity in this apparatus is essentially signature-class identity plus minor chi/maxent perturbations. Reading apparatus behavior at the metric layer alone systematically underrepresents what the rule cascade is doing.

The structural location of the finding is worth specifying. Observer-site cohomology in DR has been formally identified as genuine Abramsky-Brandenburger contextuality (*Power-Indexed Constraints Exhibit Genuine Abramsky-Brandenburger Contextuality*): H¹ > 0 corresponds to strong contextuality, and the 879 manifest-presheaf constraints in the corpus all lack global perceptual sections in the AB sense. The findings of this paper sit one layer beneath that identified contextual structure. The metric vector is the input the rule cascade evaluates; the cascade produces the type assignments whose cross-observer disagreement constitutes the contextuality. The metric layer, on the evidence collected here, does not preserve the cascade's pathway-decisions — it preserves the inputs to those decisions and discards the decisions themselves. The result is a layer that registers anomaly-region membership cleanly while saying nothing about which axiom produced the membership.

Three sections of empirical work establish this. §2 establishes that three cluster spaces are empirically distinct on the corpus, with idea space internally fractured and observer space tightly coupled to metric space. §3 establishes the central finding that within anomaly regions, the metric representation is invariant to detection pathway. §4 establishes that the disagreement between embedding-based semantic similarity and apparatus classification is systematic and interpretable, not noise. Three further sections develop the architectural consequences. §5 re-walks the trifurcation through the cluster-space lens. §6 pairs §3's finding with a corresponding result at the signature layer. §7 points toward idea-site cohomology as the natural next theoretical move. §8 concludes.

Two methodological notes are worth surfacing at the outset. First, the audit reported in this paper introduced and then retired a hypothesis about a class of constraints called "naturalized mountains" — false summits that the apparatus would tolerate due to FSM detection limitations. The retirement was data-driven: the predicted phenomenon did not appear at meaningful rate in the corpus. Where it might have lived, the apparatus has instead a small set of FCR-priority interceptions and a smaller set of authoring-driven misfires on mathematical theorems. The methodology of running an audit, recording its predictions, and acknowledging when the predictions did not hold is part of what the audit produced. Second, the paper closes with a companion-paper handoff: idea-site cohomology, which becomes well-posed once §2 and §3 establish that idea space is structurally distinct from observer and metric space.

---

#### 2. Three Distinct Cluster Spaces

The empirical foundation of this paper is a stratified sample of 265 constraints drawn from the apparatus's classified corpus, with all 34,980 constraint pairs evaluated on five disaggregated similarity measures. The sample stratifies across all six declared types — 80 mountains, 80 tangled ropes, 50 snares, 30 ropes, 11 scaffolds, 14 pitons — oversampling mountains to support the analyses in §3 and §6 and including the full populations of scaffolds and pitons. The five similarity measures are selected to operationalize the three cluster spaces with idea space disaggregated:

- *Observer-space similarity* between two constraints A and B: cosine on the type-vector across the four observer positions in the canonical site.
- *Beneficiary similarity:* Jaccard on the constraints' declared beneficiary lists.
- *Coupling similarity:* exponential decay over BFS hop distance in the contamination network (λ = 1.0).
- *Semantic similarity:* cosine on sentence-transformer embeddings of the constraints' human-readable descriptions, computed via all-MiniLM-L6-v2.
- *Metric-space similarity:* cosine on the 16-dimensional metric vector — chi at each of four observer positions, epsilon, MaxEnt distribution across the six types, and binary signature flags for the cascade's structural detectors.

The Spearman correlation matrix between the five measures across all pairs:

| | Obs | Ben | Coup | Sem | Met |
|---|---|---|---|---|---|
| Obs  | 1.000 | 0.015 | 0.009 | 0.196 | 0.776 |
| Ben  | 0.015 | 1.000 | -0.000 | 0.025 | 0.019 |
| Coup | 0.009 | -0.000 | 1.000 | 0.012 | 0.001 |
| Sem  | 0.196 | 0.025 | 0.012 | 1.000 | 0.220 |
| Met  | 0.776 | 0.019 | 0.001 | 0.220 | 1.000 |

Three readings stack.

First, observer × metric correlate at ρ = 0.776. The apparatus's "eye" — what each observer position classifies a constraint as — and its "encoding" — the metric vector that drives the rule cascade — are nearly the same function. This correlation is not a finding about the corpus; it is a finding about the apparatus. Observer-site classifications are computed from chi values at each position, which are themselves derived from the same epsilon, suppression, and theater inputs that populate the metric vector. The strong correlation reflects that observer space and metric space share inputs and operate over similar transforms of those inputs.

Second, semantic correlates with both observer and metric at only ρ ≈ 0.20. Embedding-based semantic content is mostly orthogonal to apparatus operations. The apparatus is not measuring what embedding models measure, and embedding models are not measuring what the apparatus measures. This sets up §4's finding: the gap between the two is where the apparatus's value-add lives.

Third, idea space's three sub-measures — beneficiary, coupling, semantic — are uncorrelated with each other. Beneficiary × coupling: ρ = -0.000. Beneficiary × semantic: ρ = 0.025. Coupling × semantic: ρ = 0.012. This is a stronger finding than "idea space is distinct from observer/metric space." It says there is no single latent notion of idea-relatedness that the three sub-measures triangulate. They measure different things.

The population matrix counts pair-types in three diagnostic cells:

| Cell | Count | Fraction |
|---|---|---|
| Orphan invisibility (met Q4 × idea Q1) | 1,732 | 5.0% |
| Lensing zone (idea Q4 × obs Q1) | 2,870 | 8.2% |
| Cross-cutting frame (obs Q4 × idea Q1) | 0 | 0.0% |

The orphan invisibility cell (high metric similarity, low idea similarity across all sub-measures) and the lensing zone cell (high idea similarity, low observer agreement) are the targets of §3 and §4 respectively. The cross-cutting frame cell (high observer agreement, low idea similarity) deserves a note here because it contains zero pairs, where under near-independence ~2,000 would be expected.

Semantic dissimilarity bounds observer disagreement from below. The cross-cutting frame zero says: when the four observer positions all classify two constraints similarly, those constraints share at least some semantic affinity. The apparatus does not impose unified classification patterns on semantically unrelated constraints. This is a non-pathological-behavior finding rather than a positive result — a constraint on the apparatus's hypothesis class — but it is a real one. The apparatus could in principle classify pancreatic_cancer_lethality and angular_momentum_conservation identically across all four observers (and does, since both are mountains), but only when the underlying content has some semantic resonance. Observer agreement is not free; it costs at least a thread of semantic relatedness.

Two claims close the section.

*Idea space is multi-topological, not noisy.* The three sub-measures are not three estimates of one true notion of idea-similarity. They are three different notions of neighborhood — relational (shared beneficiaries), structural (coupled in the contamination network), and conceptual (semantically near in embedding space) — that pick out genuinely different pairs as neighbors. §7 takes up the consequence: idea-site cohomology with topology-choice as a structuring decision.

*The apparatus is observer-complete and idea-incomplete.* It computes per-observer classifications and detects observer-site disagreement; it does not compute the analog at fixed observer across related constraints. Idea space exists, is structured, and is currently uncomputed except as auxiliary annotations on individual constraints. §5 develops this in the trifurcation re-walk: idea space is where Type B detection has its uncomputed counterpart.

---

#### 3. Metric Invariance to Detection Pathway

The mountain category provides the cleanest test of this paper's central claim. Axiom 3 of the framework defines mountain as natural law without beneficiary; FSM (False Summit Mountain) detection fires when a constraint claims mountain but declares beneficiaries. FCR (False CI Rope) detection fires when chi-layer Boltzmann independence detects cross-perspectival coupling alongside extraction, with Axiom 6 priority over the surface mountain claim. The two detectors operate on different inputs — FSM on declared beneficiaries (a surface check against authoring), FCR on chi-pattern coupling (a metric check against cascade thresholds) — and both can in principle fire on the same constraint.

The claim of this section: the metric layer cannot distinguish between a constraint that triggered FSM and a constraint that triggered FCR, even though the two detectors operate on different inputs and the apparatus assigns different signature flags to their outputs. The metric layer quotients out the rule cascade. Within the anomaly region around mountain, all constraints look metrically alike; the cascade's pathway-distinctions live above the metric layer.

The evidence comes from a full-corpus inventory of declared mountains and centroid analysis of the resulting groups.

##### 3.1 Mountain Inventory

The full corpus contains 425 constraints with claimed_type = mountain. These distribute across four bins by the apparatus's surface and signature criteria:

- 403 clean mountains: empty beneficiary list, no FCR signature. The Axiom 3 surface criterion holds; no FCR-priority intervention. Clean per the framework's own definitions.
- 11 FSM-caught beneficiary-having mountains: non-empty beneficiary list, signature = false_summit_mountain. The FSM detector fired on the Axiom 3 surface inconsistency.
- 3 FCR-priority caught beneficiary-having mountains: non-empty beneficiary list, signature = false_ci_rope. Both FSM and FCR could have fired; Axiom 6 priority assigned the FCR signature.
- 8 FCR-caught beneficiaryless mountains: empty beneficiary list, signature = false_ci_rope. The FSM detector had no surface inconsistency to fire on; the FCR detector fired on the chi-pattern alone.

Two notes on this inventory. The 8 beneficiaryless FCR-caught mountains were silently absorbed into a count of "clean mountains" in an earlier audit phase; the corrected count of strictly clean mountains is 403, not 411. The 78.6% FSM detection rate against its own definitional inputs (11/14 beneficiary-having mountains caught by FSM) is not a detection failure; the remaining 21.4% are FCR-priority interceptions that the apparatus produces by design (Axiom 6). The signature partition of the 14 beneficiary-having mountains is determined by Axiom 6's priority rule, not by the surface criterion that defines the inventory.

##### 3.2 Centroid Analysis

For each of five groups defined relative to the mountain inventory and the broader corpus, a 16-dimensional centroid is computed by averaging the metric vectors of the group's members. The five groups:

- G1: 403 clean mountains.
- G2: 11 FSM-caught beneficiary-having mountains.
- G3a: 3 FCR-priority caught beneficiary-having mountains.
- G3b: 8 FCR-caught beneficiaryless mountains.
- G4: 648 genuine FCR tangled ropes (claimed_type = tangled_rope, signature = false_ci_rope) — the comparison class for "what FCR-caught content looks like when it is not a mountain claim."

The 5×5 matrix of pairwise centroid cosine similarities:

| | G1 | G2 | G3a | G3b | G4 |
|---|---|---|---|---|---|
| G1  | 1.000 | 0.700 | 0.502 | 0.504 | 0.117 |
| G2  | 0.700 | 1.000 | 0.717 | 0.718 | 0.134 |
| G3a | 0.502 | 0.717 | 1.000 | 0.998 | 0.492 |
| G3b | 0.504 | 0.718 | 0.998 | 1.000 | 0.500 |
| G4  | 0.117 | 0.134 | 0.492 | 0.500 | 1.000 |

Two findings emerge.

First, sim(G3a, G3b) = 0.998. Whether a constraint declared beneficiaries or did not, conditional on FCR firing, has essentially no effect on its metric vector. The two routes that FCR can take — through declared-beneficiary structures (G3a) and through pure chi-pattern coupling (G3b) — produce centroids that are indistinguishable at the metric layer. Declared beneficiaries do not exist as a feature the metric vector encodes.

Second, all FCR-caught and FSM-caught mountains cluster together at sim ≈ 0.72 (G2/G3a/G3b pairwise) and apart from both clean mountains at sim ≈ 0.50 (G1 against any of G2/G3a/G3b) and genuine FCR tangled ropes at sim ≈ 0.49 (G4 against any of G2/G3a/G3b). The metric layer registers "anomalous mountain" as a single region. Within that region, metric similarity does not distinguish FSM from FCR pathway. Outside it, the anomalous-mountain region is well-separated from both the clean-mountain region and the FCR-tangled-rope region.

The architectural reading: within anomaly regions, the metric representation is invariant to detection pathway. The metric layer encodes anomaly-region identity, not causal provenance. Two constraints that triggered different axioms can be metrically indistinguishable. The signature layer above metric is where the apparatus's actual Type B partitioning happens.

##### 3.3 Generalization Beyond the Mountain Category

The mountain inventory is one anomaly region; the same phenomenon appears at corpus scale in the orphan invisibility cell. Of the 1,732 pairs in the cell, 1,042 (60.2%) involve constraints whose claimed types are not mountain on at least one side. The non-mountain orphan invisibility distributes across signature classes: snare × tangled_rope (362 pairs), tangled_rope × tangled_rope (256 pairs), snare × snare (127 pairs). Within each cell, pairs sit at metric similarity = 1.000 with semantic similarity ≤ 0.01 — different content, identical metric encoding, because the constraints share signature flags and chi profiles.

The mountain × mountain cell of the same population (690 pairs) is partly an algebraic consequence: the mountain category is defined to be content-invariant, so within-mountain orphan invisibility is what Axiom 3 produces. The non-mountain cells are not so easily explained. They show that signature-class metric collapse runs through the false_natural_law and false_ci_rope signature families across the corpus, not just within the mountain inventory. Wherever the apparatus has assigned a signature, the constraints carrying that signature look metrically alike regardless of content.

A single named pair makes this concrete. advice_as_dangerous_gift and central_bank_independence sit at metric similarity = 0.999 and semantic similarity = 0.061. The first constraint is about the social mechanics of advice-giving; the second is about monetary policy governance. They share no semantic content, no beneficiaries, no domain. They are metrically near-identical because they share signature-class membership. This is the cluster-space gap operationalized: the metric layer has nothing to say about the difference between advice and central banking. Whatever signature these two constraints share is what the metric layer's near-perfect similarity is registering.

##### 3.4 Architectural Implication

This connects directly to *When Consensus Isn't Coherence*'s vertical-redundancy finding. That paper argued that chi-layer detection catches what type-layer detection erases, on the basis of the cover-story mechanism: when the institutional observer's classification overrides chi-level disagreement to produce surface consensus, only multi-layer detection that spans both layers can see the structural inconsistency. The same structural point appears in this paper at a different layer pair. The metric layer encodes inputs to the cascade; the signature layer encodes the cascade's pathway-decisions. Metric similarity erases pathway distinctions because the cascade's decisions are not in the metric vector, just as type-layer agreement erases chi-level disagreement because the override mechanism resolves chi-level into type-level.

The architectural pattern: any layer in this apparatus that aggregates over a discriminative computation will collapse the discriminations the computation made. Type-layer collapses chi-layer disagreement when override mechanisms are operating. Metric-layer collapses cascade-pathway distinctions because the cascade's decisions are not encoded back into the metric vector. The apparatus's discriminative work is preserved only where it is preserved — in the chi values, in the signature flags — and downstream consumers must read those preservation points directly. Aggregated representations one layer down systematically lose the discriminations.

A third instance reinforces the pattern. The contextuality paper notes that the apparatus's MaxEnt probabilistic layer does not reproduce the discrete contextuality of the deterministic classification: H¹ and the Wasserstein distance W₁ are incommensurable, with the probabilistic layer smoothing over the strong contextuality the discrete classification carries. The same architecture: a continuous representation downstream of a discrete discriminative computation systematically loses the discriminations. The pattern holds across three independent layer pairs in the apparatus — chi/type, metric/signature, deterministic/probabilistic — and is not contingent on any one of them. It is a property of how aggregating layers relate to the discriminative computations that feed them.

---

#### 4. The Lensing Zone as Systematic Disagreement

The lensing zone is the third diagnostic cell of the population matrix: high idea similarity, low observer agreement, 2,870 pairs. The naive interpretation is that these pairs represent semantically related constraints that the apparatus classifies disparately across observer positions — genuine cross-axiom tension between embedding-based notions of similarity and the apparatus's structural classification. The alternative interpretation is that these pairs are corpus authorship artifacts: same author writing about the same phenomenon from different probe angles, producing two .pl files that look semantically identical but classify differently because the author was investigating different questions. Without ruling out the alternative, the lensing zone cannot ground further claims.

##### 4.1 Authorship Bisection

The bisection uses topic_domain prefix matching as a structural proxy for authorship intent. Two constraints with topic_domain values like "geopolitical/economic" and "geopolitical/economic_sanctions" share the prefix "geopolitical" and are flagged as likely axis-split. Constraints with no shared prefix are flagged as likely genuine cross-axiom tension. The proxy is coarse — same-author cross-domain probes will be mis-flagged as genuine-lensing, and different-author same-domain coincidences will be mis-flagged as axis-split — but its coarseness is structural rather than behavioral, and the bisection's validation comes from distributional separation between the two subsets rather than from accuracy on individual pairs.

Of 2,870 lensing-zone pairs, 45 (1.6%) match on topic-domain prefix and are flagged axis-split; 2,825 (98.4%) have no prefix match and are flagged genuine-lensing. Sensitivity range: under exact-match-only the axis-split count drops to 1; under any-shared-word matching it rises to 438 (15.3%). The conservative prefix-match definition is the smallest reasonable bisection.

The bisection separates the two subsets distributionally on metric similarity. Axis-split mean = 0.472 (std 0.243); genuine-lensing mean = 0.215 (std 0.194). Δ = +0.257. Same-topic pairs share metric features beyond what shared topic-domain alone would predict, consistent with the hypothesis that they are authoring variants of the same structural pattern. Genuine-lensing pairs show no such enrichment.

A planted anchor verifies the bisection. The pair eu_russian_asset_freeze_2025 ↔ russian_asset_freezing was identified prior to the bisection as an axis-split case from file inspection: both constraints address the post-2022 sanctions regime against Russian state assets, but one focuses on the EU's specific policy configuration and the other on the broader sanctions enforcement mechanism. Their topic_domain values are "geopolitical/economic" and "geopolitical/economic_sanctions" respectively. The bisection places this pair in the axis-split subset (semantic = 0.716, metric = 0.684, topic_match = 0.5 under prefix "geopolitical"), confirming the proxy operates as expected on a planted positive case.

##### 4.2 What the 98.4% Are

The genuine-lensing finding rules out the authorship-artifact interpretation. The remaining question is what the genuine-lensing pairs are. The top-ranked genuine-lensing pairs by semantic similarity, with at least one mountain or tangled rope on the side, share a clear pattern:

- prisoners_dilemma_equilibrium (mountain) ↔ nash_equilibrium_coordination (rope), sem = 0.681, met = 0.068.
- finite_group_classification (mountain) ↔ collective_action_problem (rope), sem = 0.461, met = 0.100.
- vehicle_routing_problem_distance_symmetry (mountain) ↔ quadratic_assignment_symmetry_handling (rope), sem = 0.460, met = 0.090.
- lyapunov_exponent_computation (mountain) ↔ cryptocurrency_velocity_dynamics (tangled_rope), sem = 0.400, met = 0.192.

The pattern: mathematical or physical natural laws semantically adjacent to their coordination-mechanism applications. The Prisoner's Dilemma's mathematical equilibrium structure is a mountain — natural law without beneficiary, derivable from game-theoretic axioms, position-invariant. Nash equilibrium coordination as a structure people implement in real institutions is a rope — coordination mechanism with low extraction, observer-dependent, classifiable. The two share most of their conceptual surface — both involve players, payoffs, equilibria, strategic interaction — and embedding-based similarity reflects that shared surface at sem = 0.681. The apparatus reads through the surface to distinguish the mathematical fact from the institutional implementation, and the resulting metric similarity is 0.068.

This is not a failure of either system. It is a systematic difference in what is being measured. Embedding models compute phenomenon-level similarity: do these two constraints discuss similar things? The apparatus computes axiomatic role: does each constraint, under the framework's classification axioms, occupy the position of a natural law or a coordination mechanism or an extractive structure? The two questions can have correlated answers — and over the corpus they do, at ρ = 0.220 — but they can also diverge sharply, and where they diverge the divergence is not noise.

The lensing zone is where the apparatus earns its keep. The orphan invisibility cell of §3 is where the apparatus has nothing to say about content distinctions; the lensing zone is where the apparatus has something to say that embedding-based similarity cannot. In the orphan invisibility cell, advice_as_dangerous_gift and central_bank_independence look identical because they share signature class. In the lensing zone, prisoners_dilemma_equilibrium and nash_equilibrium_coordination look identical to the embedding model and very different to the apparatus, because the apparatus is reading something the embedding does not.

The two cells are reciprocal failures of one notion of similarity to capture what the other captures. Neither is more "true" than the other; they measure different things. But the asymmetry of what each notion misses is informative. Where embedding models miss the axiomatic role distinction, the apparatus surfaces it. Where the apparatus misses the content distinction, embedding models surface it. The two cells together show that the apparatus and embedding-based semantic similarity are non-redundant tools, each picking up structure the other erases.

---

#### 5. The Trifurcation Re-Walked Through Cluster Spaces

The trifurcation classified failure modes by which collapse function went wrong: drift across views (Type A), inconsistency within a view (Type B), underspecification of which view (Type C). The cluster-space taxonomy classifies failures by which space the failure lives in. The two cuts are not coordinate. Each failure mode can in principle occur in each cluster space, and the apparatus's coverage across the 3×3 is uneven in ways that surface what the apparatus does and does not measure.

| | Observer space | Idea space | Metric space |
|---|---|---|---|
| Type C (specification) | rich (gauge orbits, H¹) | thin | nominal |
| Type B (structure)     | rich (FCR/FSM/FNL, signature cascade) | thin (this paper opens this) | nominal |
| Type A (drift)         | thin (drift-events module) | thin | thin |

Type C lives primarily in observer space. The original Type C definition — indexical underspecification — resolves through specification of which observer position is being queried. The apparatus's gauge-orbit machinery and cohomological obstruction computation operate on observer space directly. *When Splitting Isn't Solving* developed this cell: specification produces clean splits (sheaves) or structured splits (presheaves), and the H¹ gap structure, Arakelov height, and product-site stability are all observer-space measurements. The apparatus's Type C coverage is rich in observer space and effectively absent in idea space (no cohomology of a fixed observer's classifications across related constraints) or metric space (specification operates on observer indices, not metric coordinates).

Type B lives primarily in idea space, but the apparatus's coverage is observer-side. Type B failures are inconsistencies between a constraint's claims and the structural axioms — the apparatus has FCR, FSM, FNL, Boltzmann compliance, and the structural-signature cascade as detectors. These detectors operate per-constraint at fixed observer positions, then aggregate. They detect lens-internal tensions in the conceptual structure being viewed, but they do so by checking each constraint individually against axioms, not by checking whether a given observer's classifications across related constraints form a coherent worldview. Idea-side Type B — H¹ on the idea site with observer fixed — is conceptually well-defined and currently uncomputed. §7 takes up this gap.

A note on related work. Some idea-space-adjacent phenomena are measured under different framings outside the trifurcation. The contamination network's purity propagation, the super-spreader analysis, and the network-level cover-story extension (*When Nodes Aren't the Unit*) measure aspects of how constraints' classifications relate across the coupling graph. These are characterized in a prior audit as "outside the trifurcation's taxonomy" because the trifurcation operates on individual reasoning episodes; network-level phenomena are a genuine extension. The cluster-space framing developed here makes idea space a first-class domain rather than an extension, but the existing network-level work occupies overlapping territory under a different framing. Where this paper says idea-side Type B is uncomputed, what it means precisely is that idea-site cohomology with observer fixed is uncomputed; some idea-space-adjacent measurements exist under network-extension framings.

Type A lives in time-direction movement of either. Type A failures are unmarked drift across cluster spaces over time. The apparatus's drift-events module catches a few specific patterns when measurement history exists (metric substitution, extraction accumulation, coordination loss, sunset violations, Boltzmann-property drift), but in the cluster-space framing Type A is more general: any unmarked traversal of any cluster space. *When Frame Isn't Foreground* makes the parallel architectural point — Type A is granularity-promiscuous, distributing across whatever level frame-comparison happens at — and the cluster-space framing extends this: Type A also distributes across whatever cluster space the frame-comparison happens in. The current corpus does not carry sufficient temporal data to compute Type A coverage cleanly across cluster spaces; this is a deferred direction.

The asymmetry-of-failure-types result generalizes through the cluster-space lens. *Asymmetry of Failure Types* argued that the three types correspond to three different relations between failure and formalism — contradiction-to-structure (Type B), contradiction-to-specification (Type C), contradiction-to-governance (Type A). The cluster-space framing adds: each of these relations can in principle hold across multiple cluster spaces, and the apparatus's coverage of the resulting matrix is uneven not by accident but by which spaces the apparatus is observing. The summary line: the apparatus is observer-complete and idea-incomplete. Across all three failure modes, the apparatus's primary instruments operate on observer space. Idea space is where the apparatus's most underdeveloped diagnostic capacity lives, and the next theoretical move is taking idea space seriously as a site for cohomology computation in its own right.

---

#### 6. Apparatus Self-Knowledge: Trigger-Reading vs. Signature-Reading

The §3 finding (metric layer collapses pathways into anomaly regions) has a parallel at the signature layer that this section develops. The same architectural pattern repeats one level up: signature identity does not preserve detection-event identity. A signature flag is an output class; the cascade produced it through a particular pathway, and different pathways can produce the same flag. Reading the apparatus's behavior at the signature layer alone — treating false_ci_rope as a unitary detection event — collapses pathway distinctions just as reading at the metric layer collapses signature distinctions.

##### 6.1 The Misfire Pattern

The mountain inventory in §3 surfaced 8 constraints with claimed_type = mountain, empty beneficiaries, and signature = false_ci_rope. The FSM detector had no surface inconsistency to fire on (no declared beneficiaries to flag); the FCR detector fired on the chi-pattern alone. These 8 plus the 3 with declared beneficiaries form the full FCR-caught mountain population (11 constraints).

These 11 constraints distribute across three discrete operating points of the rule cascade, defined by epsilon value: ε = 0.18 (1 case: thai_article_112_mountain), ε = 0.12 (5 cases: a cluster of mathematical theorems), ε = 0.08 (2 cases: prisoners_dilemma, spacetime_realism). Within each epsilon group, the chi vectors across the four observer positions are identical to four decimal places. FCR-on-mountains is not catching a heterogeneous structural pattern; it is firing at three specific rule-cascade thresholds where epsilon, suppression, and theater inputs produce particular chi-spread profiles that match the FCR trigger condition.

File-level inspection of named cases reveals that the same signature flag covers structurally different detection events.

The thai_article_112_mountain case is genuine ambiguity. The author explicitly coded a powerless-snare perspective alongside the mountain claim, producing a real perspectival fracture in the constraint's own data. The FCR detector reads this fracture as cross-perspectival coupling and fires; the mountain claim survives the cascade because the rule was evaluated against pre-FCR metrics. The signature is doing its detection job: there is structural inconsistency in the constraint, and the apparatus has flagged it.

The yoneda_lemma and boltzmann_universality_2026 cases are different. Both are mathematical theorems — pure mathematical structures with no extractive content, no beneficiaries in any plausible reading, no coordination structure outside the abstract. Their authoring populates four perspectives in the .pl format because the corpus convention requires four perspectives. The natural way to write four perspectives on a theorem is to position one observer as institutional (the mathematical orthodoxy that endorses the theorem) and another as outside-researcher (someone investigating the theorem's foundations or limits). This authoring choice produces a chi-spread pattern that matches FCR's trigger shape, and FCR fires. The signature is the same as on thai_article_112 (false_ci_rope), but the underlying detection event is structurally different: FCR has detected a perspective-coupling pattern that exists in the .pl file's metadata, not in the constraint's content.

The spacetime_realism case is intermediate. The author wrote seven perspectives covering a wider range of observer types (tangled_rope, snare, piton among them). The richer perspective set produces the smallest chi spread within its epsilon group, and FCR's detection signal is correspondingly weaker. Diverse perspectives absorb the FCR signal into a less-sharp pattern.

##### 6.2 The Pairing with §3

A signature flag of false_ci_rope can mean three structurally different things:

(a) Genuine cross-perspectival coupling on extraction, the case Axiom 6 was written to handle. The apparatus is detecting what it was designed to detect.

(b) Authoring-driven perspective split that matches the FCR trigger shape, when the constraint's content does not contain extraction structure but the .pl file's four-perspective coverage produces a chi-spread pattern that does. This is an apparatus misfire on a corpus-convention artifact.

(c) Borderline cases where some real structural feature in the constraint produces an FCR-trigger-shaped chi spread, but the structural feature is not the kind of cross-perspectival coupling Axiom 6 targets.

These three cases produce identical signature flags. A diagnostic that consumes only signature flags treats them as equivalent. A diagnostic that reads what triggered the signature — what chi-pattern fired the FCR detector, what authoring choices produced the chi-pattern, what content the chi-pattern reflects — can distinguish detection success from misfire from boundary case.

§3 showed that the metric layer collapses pathway distinctions into anomaly-region identity. §6 shows that the signature layer collapses pathway distinctions into output-class identity. Different layers, same structural pattern: an aggregating layer downstream of a discriminative computation systematically erases the discriminations the computation made. The cascade's pathway decisions are not encoded in the metric vector beneath it; they are not encoded in the signature flag above it either. The pathway exists in the cascade itself, and reading the apparatus's behavior at any layer other than the cascade requires recognizing that the layer one is reading is a downstream summary, not a record.

##### 6.3 Methodological Consequence

Apparatus self-knowledge therefore requires reading triggers, not signatures or metrics in isolation. A signature is an output class; a trigger is what activated it; the trigger can be a cascade pathway, an authoring artifact, or a boundary case. The apparatus's diagnostic literature has tended to treat signatures as detection events. The misfire pattern shows this is too coarse: the same signature can name multiple kinds of detection event, and which kind happened in any given instance is a question the signature alone does not answer.

A trigger-aware diagnostic should report (1) which axiom or rule fired, (2) what input pattern matched the rule's trigger condition, (3) what authoring choices in the .pl file contributed to that pattern. The current apparatus reports (1) reliably, (2) implicitly through inspection of the metric vector, and (3) not at all. The third category — authoring contribution to detection — is the new diagnostic surface that the misfire finding makes legible.

The misfire pattern is not, in this framing, a separate failure category to add to the apparatus's taxonomy. It is an instance of the broader principle that detection-event identity is finer than signature-flag identity. Mathematical theorems are vulnerable to FCR misfire because the corpus convention requires four perspectives that mathematical content does not naturally have, and the resulting authored split happens to match the FCR trigger shape. The lesson is methodological rather than taxonomic: read triggers, not signatures.

---

#### 7. Toward Idea-Site Cohomology

The empirical findings of §2 establish that idea space exists as a structured domain in the corpus, distinct from observer and metric space and internally fractured into three uncorrelated sub-topologies. The architectural finding of §3 establishes that the apparatus's measurement of structural pattern at the metric layer is essentially routing rather than measurement of relational structure. Together, these findings make idea-site cohomology the natural next theoretical move, but they also constrain how that move has to be made.

Observer-site cohomology in the framework's existing apparatus has been formally identified as genuine Abramsky-Brandenburger contextuality (*contextuality_paper*). The four canonical observer positions form a measurement scenario with K₄ compatibility structure; the orbit vector is the empirical model; H¹ > 0 corresponds to logical impossibility of a global section. Strong contextuality holds: 879 manifest-presheaf constraints all lack global perceptual sections, with the snare contextuality fraction at 1.000. The apparatus's H¹ is not a structural analogy to AB contextuality; it is an instance of it.

The dual question is: for a fixed observer, how do classifications across related constraints fail to glue into a coherent global section? H¹ on an idea site, with observer fixed, would measure observer-internal incoherence across the network of related constraints — whether a worldview holds together at this position, given the apparatus's classifications and the relational structure of the corpus.

This question is not currently uncomputable in the same way it was uncomputable before the cluster-space audit. The first task is not computation but specification. The AB framework requires (X, O, M): measurements, outcomes, compatibility structure. For observer-site cohomology, X is observer positions, O is the type space, M is K₄. For idea-site cohomology, X is constraints, O is the type space at the fixed observer, and M is — what? The compatibility structure for idea-site cohomology is the structuring decision the next paper has to make, and the §2 finding that idea space is multi-topological means the choice is real rather than notational. The three sub-measures (beneficiary, coupling, semantic) are uncorrelated; they are not three estimates of one true notion of idea-relatedness; they are three different notions of neighborhood. Each defines a candidate site:

*Beneficiary-overlap topology.* Two constraints are neighbors if they share at least one declared beneficiary. The omega_cross_constraint module already computes related quantities — extractive type counts within beneficiary groups, scenario-convergence patterns. Population: 32.5% of corpus has non-empty beneficiary lists; cohomology computes on that subset.

*Coupling-graph topology.* Two constraints are neighbors if they are connected in the contamination network. *When Nodes Aren't the Unit* identifies the formal definition of a presheaf on this site as Ω₃ — an unresolved formalism question for the network-level cover-story extension. Idea-site cohomology over the coupling graph would address the same formalism question from a different angle, by asking what structure on the coupling graph would make global sections meaningful.

*Semantic-embedding topology.* Two constraints are neighbors if they are nearest-neighbors in embedding space. The §2 finding of ρ ≈ 0.20 between semantic and observer/metric similarity means a semantic-site cohomology will report on relations the apparatus is not currently observing.

Each topology yields a different cohomology and likely a different paper. The choice of which to develop first is a paper-shaping question that the cluster-space audit does not answer; it surfaces the choice as a real choice rather than a notational preference. Whether any of the three yields an AB-style contextuality result depends on whether the chosen compatibility structure admits the consistency conditions the AB framework requires. This is open. Idea-site cohomology might be a different formal object than observer-site cohomology, or it might be a parallel instance of contextuality with different physics. Specification of the formal structure has to precede computation.

Once an idea-site is chosen and H¹ on it is computed for each observer position, a question becomes available that the apparatus cannot currently ask: does observer-internal coherence (low idea-site H¹ at U₃, the institutional position) come at the cost of cross-observer disagreement (high observer-site H¹)? If yes, the apparatus measures a tradeoff between cluster spaces — coherence purchased at one space paid for at another — and the trifurcation's separation between Type B (idea-side) and Type C (observer-side) failure modes might be tradeoffs against each other rather than independent dimensions.

This is currently a hypothesis, not a result. It cannot be tested without the idea-site cohomology computation, which itself cannot be performed without first specifying the formal structure of the idea site. The work is sequenced: specification, then computation, then comparison against observer-site H¹. The cluster-space audit provides the empirical foundation for that sequence and surfaces the first decision as a structuring choice rather than a notational preference. The companion paper takes it up.

---

#### 8. Conclusion

The cluster-space audit develops three empirical findings and one architectural claim. The three findings: cluster spaces are empirically distinct on the corpus, with idea space internally fractured; the metric layer is invariant to detection pathway within anomaly regions; the lensing zone is genuine cross-axiom tension between embedding and apparatus, not authorship artifact. The architectural claim: the apparatus's metric layer is a routing mechanism that pre-partitions constraints by anomaly region, and the actual discriminations happen one layer up in the rule cascade. Reading apparatus behavior at the metric layer alone underrepresents what the rule cascade is doing.

Two extensions of this claim follow. First, the same architectural pattern repeats at the signature layer: signature flags collapse pathway distinctions into output classes, and the misfire pattern on mathematical theorems shows that signature identity does not imply detection-event identity. Apparatus self-knowledge requires reading triggers, not signatures or metrics in isolation. Second, the audit retired a hypothesis about naturalized mountains as an unmeasured class of false summits. The retirement was data-driven: the predicted phenomenon did not appear at meaningful rate, and what the apparatus has instead is a small set of FCR-priority interceptions and a smaller set of authoring-driven misfires. The audit falsified a plausible alternative explanation for anomaly clustering, which is itself an evidentiary contribution.

The forward direction the audit makes available is idea-site cohomology with topology choice as the structuring decision. The §2 finding that idea space is multi-topological means there is no single "right" idea-site to cohomologize over; there are three uncorrelated candidates, each yielding a different theoretical object and a different empirical trajectory. The cluster-space audit provides the empirical foundation for that next move and surfaces the choice as a real one. The companion paper takes it up.

---

#### References

Internal to the project:

- *Debugging Philosophy: A Trifurcation Framework for Paradox Classification.* The source taxonomy of A/B/C failure types this paper re-walks through the cluster-space lens.
- *Deferential Realism: A Presheaf Framework for Observer-Dependent Classification* (v6.11). The apparatus paper. Source for Axiom 3 (six-type space), Axiom 6 (FCR priority), the FCR/FSM/FNL detectors, the signature cascade, and the corpus this paper audits.
- *Observers, Not Humans: Deferential Realism Across Observer Classes* (v5). Source for the parametric-vs-epistemic fragility distinction and the universality-class framing.
- *When Splitting Isn't Solving: Sheaves, Presheaves, and the Structure of Indexical Disagreement.* The Type C extension paper. Source for the sheaf/presheaf criterion this paper takes for granted in §5.
- *When Consensus Isn't Coherence: The Cover-Story Mechanism and Multi-Layer Type B Detection.* The Type B extension paper. Source for the vertical-redundancy claim that §3 grounds empirically at the metric/signature layer pair.
- *When Frame Isn't Foreground: Type A and the Limits of Module-Level Audit.* The Type A architectural-reframe paper. Source for the granularity-promiscuity claim §5 extends to cluster spaces.
- *When Nodes Aren't the Unit: Network-Level Cover Stories and the Limits of Single-Constraint Detection.* The network-level cover-story extension. Source for the characterization of network-level phenomena as "outside the trifurcation's taxonomy," the super-spreader near-clique finding, and Ω₃ as the formalism question idea-site cohomology has to address.
- *Power-Indexed Constraints Exhibit Genuine Abramsky-Brandenburger Contextuality.* The contextuality-identification paper. Source for the formal identification of observer-site cohomology with AB strong contextuality, the snare contextuality fraction = 1.000 result, and the H¹/W₁ incommensurability finding §3.4 cites as a third instance of the architectural pattern.
- *The Asymmetry of Failure Types: Detection, Prevention, and Governance in Analytical Validation.* The meta-companion paper this paper extends through the cluster-space lens.
- *Metrics as Routing: Why Thresholds Are Governance Stands, Not Truth Claims.* The foundational document whose first-principles claim §3 grounds empirically.
- Cluster-Space Audit Phases 1–5 (internal, 2026). Source for all empirical findings reported here.

---

*Working paper. Conceptual development in conversation with Claude (Anthropic). The cluster-space taxonomy emerged from the recognition that the apparatus's primary visible work — observer-site disagreement detection — is one of three empirically distinguishable cluster spaces, and that idea space and metric space are structurally distinct from observer space and from each other. The five-phase audit was conducted under a discipline of evidence collection before interpretation, with each phase's hypothesis recorded before the data was examined. The naturalized-mountain hypothesis introduced in early phases and retired in Phase 5 is reported here as part of what the audit produced; the methodology of recording predictions and acknowledging when they did not hold is consistent with the trigger-reading discipline §6 develops.*
