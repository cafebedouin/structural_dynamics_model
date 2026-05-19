# DR Apparatus: Future Work Agenda

## Current state (May 18, 2026)

The apparatus shipped substantial additions through v6.12:
- CS pattern detection (prolog/cs_pattern_detection.pl) with six patterns 
  and verdict layer
- UKE_SCOPE §1.3 commitment system recognition
- Generation prompt CS Structure section + framing-omega invitation
- Schema additions (cs_structure object, optional)
- Orchestrator handling of commitment_system_recognition manifest field
- Enhanced report L2 CS pattern section
- Empirical validation via Russian command authority case
- v6.12 documentation closing the gap with three architectural commitments 
  named: verdict-layer pattern (correction-grade vs commentary-grade), 
  optional-schema-with-verdict-accountability, level-of-analysis distinction

What's been deferred: predictive machinery from commitment_systems_sketch_v3 
that was trimmed in v4 (cover stories per pattern, lifecycle phases, 
decoupled formalization, ritualized renewal). Clustering exploration over 
predicate space. δ-replacement / baseline-deviation reframing. Cluster-level 
CS inference architecture.

## Work packages in priority order

### Package 1: v3 prediction machinery extension

**What:** Operationalize the predictive scaffolding from commitment_systems_
sketch_v3 that was trimmed in v4. Specifically:
- Cover stories per CS pattern (each pattern predicts characteristic 
  cover-story signatures that should be detectable in constraint data)
- Lifecycle phases as distinct stages with diagnostic signals (which phase 
  is the CS in, what comes next)
- Decoupled formalization flag (when formal kernel and actual operational 
  authority are at different locations in the system)
- Ritualized renewal success/failure (whether sunset-clause mechanisms 
  actually exercise vs. perform exercise)

**Why this first:** The v6.12 work documented CS classification capability 
but the apparatus can't yet do trajectory prediction. The v3 material 
specifies what prediction would look like. Operationalizing it gives the 
apparatus forecast capacity, not just descriptive capacity. The 
collective_action_as_leverage_conversion constraint (the lone 
triple-confirmed liminal case in the corpus) is the natural first empirical 
case — it sits unresolved with high entropy and active drift, exactly the 
case prediction machinery would characterize.

**Architectural shape:** Each predictive feature gets a detector predicate 
following the same LLM-asserts/math-comments pattern. Lifecycle phase 
detection would be commentary-grade (annotates without overriding). 
Cover story detection could be commentary-grade or correction-grade 
depending on confidence threshold. Decoupled formalization probably 
needs a new optional schema field (kernel_location vs. operational_
authority_location) with verdict-layer accountability per the v6.12 
optional-schema pattern.

**Deliverable:** Extension of cs_pattern_detection.pl (or new module 
cs_prediction.pl) with the v3 features operationalized, plus a CS paper 
section documenting the predictive capability with the 
collective_action_as_leverage_conversion case as worked example.

**Sequencing:** Audit the v3 sketch against current apparatus capabilities, 
identify which v3 features can be operationalized with existing predicates 
and which need new schema fields. Design discriminator logic per feature 
following the cs_pattern_detection.pl precedent. Implement with the 
correction-grade vs. commentary-grade discipline. Test against 
collective_action_as_leverage_conversion and against the existing Russian 
command authority case (which should produce consistent predictions if 
the apparatus is working).

### Package 2: Empirical run on second and third cases

**What:** Run two additional cases through the full apparatus once 
Package 1 ships:
- 2026 US midterm constitutional legitimacy axes
- Colombia 2026 election (Mountain/Rope/Noose case from existing analysis)

**Why this second:** The Russian case validated the architecture but is 
one empirical point. Two more cases — one with partial CS recognition 
(midterms, where some axes are CS-relevant and some aren't) and one 
where the analyst has already done Mountain/Rope/Noose analysis 
(Colombia, where the apparatus output can be compared to your existing 
analysis) — produces calibration evidence and stress-tests the 
discrimination logic. The Colombia case is particularly useful because 
the existing analysis already characterizes the constraint topology; 
the apparatus's output should converge on similar findings or diverge 
in informative ways.

**What to watch:** Whether the CS pattern detection correctly fires on 
constitutional-legitimacy axes and correctly omits cs_structure on 
vote-count mechanics. Whether Colombia's Mountain (constitutional term 
limit), Rope (Pacto Histórico coalition), and Noose (two-round runoff 
forcing bipolar compression) produce constraint stories with cs_structure 
populated where appropriate. Whether v3 prediction machinery surfaces the 
"terminal attractor" framing that the existing Colombia analysis 
produced manually.

**Deliverable:** Two constraint story sets, two enhanced reports, two 
essays, plus calibration notes on where the apparatus matched, exceeded, 
or fell short of the existing analyses.

### Package 3: δ → baseline-deviation reframing (v6.13 or v7.0)

**What:** Replace the δ (cognitive displacement) parameter with a more 
precisely-factored framework:
- CS architecture absorbs the structural component of intra-position 
  disagreement (kernel-vs-operation framing variance, diffuse 
  reconstruction predictions, asymmetric acknowledgment standing)
- A narrower residual parameter captures empirical deviation from baseline 
  given position (superforecaster calibration deviation, domain-expert 
  deviation within domain, identity-bound investment deviation)
- The reframed parameter is operationalized as measurable deviation from 
  population baseline rather than as psychological adjustment

**Why this third:** Substantive theoretical move. Requires the v3 prediction 
machinery (Package 1) to be in place because CS architecture needs to 
already be doing predictive work before its absorption of δ's structural 
work can be evaluated. The reframing isn't a vocabulary swap; it changes 
what evidence the framework needs. Worth doing carefully.

**Architectural shape:** δ in the existing apparatus is a parameter; the 
replacement is more like a deviation-measurement layer that takes 
observer characteristics as input and produces calibrated deviation from 
expected position. The CS architecture handles structural variance; the 
deviation layer handles empirical variance. This is a v6.13 or v7.0 
paper-level revision, not a workman update.

**Sequencing:** Theoretical work first (specify what's structural and what's 
residual after CS does its work). Apparatus changes second (the deviation 
layer needs design before implementation). Documentation third (substantial 
paper revision).

### Package 4: Cluster-level analysis

**What:** Add corpus-statistics aggregation over beneficiary clusters as 
L3 corpus-positioning content:
- Cluster signature statistics (% convergent_institutional, % convergent_
  drift, aggregate purity, drift trajectories)
- Cluster-level CS inference as analytical move (given cluster statistics, 
  what domain-level CS would account for the convergent pattern?)
- Bridge from cluster-level CS inference back to constraint-level 
  cs_structure (when cluster has likely kernel, do member constraints' 
  cs_structure declarations match?)

**Why this fourth:** The convergent_institutional findings already surface 
cluster-shaped phenomena (free_riders, status_quo_preservers, algorithmic_
platforms etc.). The apparatus surfaces them as findings but doesn't 
produce cluster-level statistics or cluster-level CS analysis. Adding 
this preserves the level-of-analysis distinction from v6.12 — cluster 
findings are corpus-level outputs, not per-constraint reclassifications.

**Architectural shape:** Mostly Python work in enhanced_report.py + 
pipeline_output.json corpus-statistics aggregation. Some Prolog if 
cluster-level CS inference becomes detectable. Likely doesn't need 
schema changes — the cluster is a derived object from beneficiary 
groupings.

### Package 5: Systematic clustering exploration

**What:** Cluster the corpus over predicate space (feature vectors built 
from existing facts: ε, suppression, theater_ratio, signature, 
coordination_type, beneficiary count, victim count, network density, 
drift event types, perspectival fracture H¹, Boltzmann compliance, etc.). 
Surface candidate clusters that don't map cleanly to existing taxonomy. 
Interpret cluster candidates with theoretical priors to identify the 
1-3 (out of likely 10-25 candidates) that suggest unnamed patterns 
worth developing.

**Why this fifth:** Exploratory research. Benefits from the apparatus 
running on more corpus material so the clustering has more data to 
work with. The development process for any pattern that emerges would 
follow the commitment_systems sketch v1→v2→v3→v4 pattern — months of 
iteration before operationalization. Not urgent but potentially 
high-value.

**Architectural shape:** Data analysis script, not a Prolog module. 
Output is "here are N candidate clusters with their predicate signatures 
and member constraints" for human interpretation. Premature Prolog-
ification would produce noise; the interpretive work is the point.

**Dependencies:** Benefits from Package 1 (more capabilities mean richer 
predicate space) and Package 4 (cluster-level analysis primes the 
interpretive framework). Probably runs after Packages 1-4 have shipped 
and the corpus has accumulated 3-6 months of additional growth.

## Disciplines to maintain across packages

- **Existing-apparatus-first:** When new findings emerge, ask what the 
  existing apparatus already says before proposing new architecture. 
  Most "we need to extend X" instincts can be resolved as "the apparatus 
  already produces Y; the work is interpretation, not extension."

- **Documentation cadence:** Roll workman documentation updates every 
  major capability addition rather than letting drift accumulate. v6.11 → 
  v6.12 had to close roughly four months of accumulated additions; future 
  rolls should be smaller and more frequent.

- **Level-of-analysis preservation:** The engine classifies constraints, 
  the authoring protocol characterizes domains, cluster analysis 
  characterizes corpus structure. Don't conflate these levels by 
  applying constraint-level detection to cluster-level patterns or 
  vice versa.

- **Working-notes vs. apparatus distinction:** Theoretical sketches 
  (commitment_systems_sketch v4, eventual v3-extension successors) 
  remain working notes; operationalized capabilities live in code and 
  formal paper. Don't let sketches become apparatus documentation or 
  vice versa.

- **Verification before claiming:** All documentation claims about 
  current code must be verified against code before being written into 
  the paper. v6.12 audit caught two cases where v6.11 was already 
  correct contrary to expectation; the discipline produced clean 
  findings.

- **Empirical material before architectural extension:** Running the 
  apparatus on more cases is higher-leverage than building more 
  architecture. Package 2 should run before Packages 3-5 are 
  prioritized; the calibration evidence shapes what's worth building.

## Cases queued for empirical runs

- `collective_action_as_leverage_conversion` — the lone triple-confirmed 
  liminal case. Should be the first case run through Package 1's v3 
  prediction machinery. Already in the corpus.
- 2026 US midterm constitutional legitimacy axes (Package 2)
- Colombia 2026 election (Package 2)
- Iran, Portugal, Thailand, Bangladesh election analyses from prior 
  superforecasting work (Package 2 follow-on)
- Religious doctrinal case (canonical interpretive_accretion test)
- 2026 ISW reports as ongoing CS analysis material

## Open questions worth tracking

- Whether the framing_notes-shaped invitation in the generation prompt 
  reliably produces conceptual omegas vs. empirical-leaning omegas. The 
  Russian case produced omega_gerasimov_awareness which did the right 
  analytical work but was framed as epistemic rather than framework-
  choice. Calibration signal, not implementation gap, but worth watching.

- Whether the convergent_institutional pattern (institutional sees rope, 
  analytical sees snare) is itself a CS-detectable signature at the 
  cluster level (Package 4 question).

- Whether v3's lifecycle phases produce useful predictions on cases 
  where the existing apparatus produces ambiguous terminal-state 
  predictions (collective_action_as_leverage_conversion is the test 
  case).

- Whether δ-replacement produces operational improvements (deviation 
  measurements that wouldn't be possible with δ as parameter) or is 
  primarily a theoretical reframing.

## Out of scope (deliberate)

- Reform-intervention recommendations from cluster findings. The 
  apparatus surfaces patterns; intervention work is human analytical 
  work that uses the patterns as input. Don't add intervention 
  prescription to the apparatus.

- Forcing every constraint to have cs_structure populated. The optional-
  schema pattern is load-bearing; constraints that aren't commitment 
  systems should silently omit cs_structure rather than declare 
  cs_structure: not_applicable.

- Reclassifying constraints based on cluster membership. The cluster 
  finding is information about constraint relations, not information 
  that should override individual classifications. The observer-
  dependence the apparatus preserves is the structural finding, not a 
  defect to be aggregated away.
