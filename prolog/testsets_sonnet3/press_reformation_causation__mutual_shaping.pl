% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__mutual_shaping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__mutual_shaping, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: press_reformation_causation__mutual_shaping
 *   human_readable: The Press-Reformation Co-Evolution Scaffold (Mutual Shaping Reading)
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This story instantiates the mutual_shaping reading of the
 *   press_reformation_causation kernel: neither the printing press alone
 *   determined the Reformation's course (technological_determinism), nor did
 *   reformers simply pick up a neutral, already-formed tool and deploy it
 *   (strategic_deployment). Instead, reformers' demand for cheap, fast,
 *   vernacular distribution pushed printers to develop new formats, business
 *   models, and typesetting conventions, and those material developments in
 *   turn expanded what reformers could rhetorically and organizationally
 *   attempt next — serialized disputations, mass-produced woodcut polemic,
 *   coordinated multi-city release of tracts. The press functioned as a
 *   scaffold: an enabling structure reformers reinforced through use, which
 *   is why it is claimed here as scaffold rather than mountain (it was not an
 *   immutable natural fact) or pure rope (coordination benefit was real but
 *   asymmetrically distributed against scribes and censors, and enforcement
 *   against unlicensed printing was a genuine active suppression mechanism,
 *   not mere friction).
 *
 * KEY AGENTS:
 *   - protestant_reform_networks: primary co-shaping agent (organized/mobile) — drove format and distribution innovation
 *   - printer_publisher_guilds: primary co-shaping agent (organized/mobile) — adapted production to reformist demand and thereby reshaped what reform could attempt
 *   - catholic_ecclesiastical_censors: primary institutional target (institutional/constrained) — bore rising enforcement costs against a moving target
 *   - displaced_scribal_copyists: structural casualty (powerless/trapped) — no coordination benefit, pure displacement
 *   - historians_of_technology: analytical observer — arbitrates among the three kernel readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__mutual_shaping, 0.38).
domain_priors:suppression_score(press_reformation_causation__mutual_shaping, 0.42).
domain_priors:theater_ratio(press_reformation_causation__mutual_shaping, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, extractiveness, 0.38).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__mutual_shaping, scaffold).
narrative_ontology:human_readable(press_reformation_causation__mutual_shaping, "The Press-Reformation Co-Evolution Scaffold (Mutual Shaping Reading)").
narrative_ontology:topic_domain(press_reformation_causation__mutual_shaping, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(press_reformation_causation__mutual_shaping).
narrative_ontology:has_sunset_clause(press_reformation_causation__mutual_shaping).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__mutual_shaping, '2369534b-858d-41ce-b413-fee9e042aec5').
narrative_ontology:cs_kernel_codification('2369534b-858d-41ce-b413-fee9e042aec5', distributed).
narrative_ontology:cs_authority_grounding('2369534b-858d-41ce-b413-fee9e042aec5', distributed).
narrative_ontology:cs_reading_relation('2369534b-858d-41ce-b413-fee9e042aec5', press_reformation_causation__technological_determinism, coexists_with).
narrative_ontology:cs_reading_relation('2369534b-858d-41ce-b413-fee9e042aec5', press_reformation_causation__strategic_deployment, coexists_with).
narrative_ontology:cs_axiom('2369534b-858d-41ce-b413-fee9e042aec5', foundational, technology_and_practice_are_co_constitutive).
narrative_ontology:cs_axiom_status(technology_and_practice_are_co_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('2369534b-858d-41ce-b413-fee9e042aec5', technology_and_practice_are_co_constitutive, empirically_contingent).
narrative_ontology:cs_axiom('2369534b-858d-41ce-b413-fee9e042aec5', secondary, neither_material_affordance_nor_agency_has_causal_priority).
narrative_ontology:cs_axiom_status(neither_material_affordance_nor_agency_has_causal_priority, holdable).
narrative_ontology:cs_axiom_grounding('2369534b-858d-41ce-b413-fee9e042aec5', neither_material_affordance_nor_agency_has_causal_priority, empirically_contingent).
narrative_ontology:cs_reference_frame('2369534b-858d-41ce-b413-fee9e042aec5', co_evolutionary_scaffold_frame).
narrative_ontology:cs_drift_state('2369534b-858d-41ce-b413-fee9e042aec5', contemporary_historiography, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('2369534b-858d-41ce-b413-fee9e042aec5', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__mutual_shaping, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, protestant_reform_networks).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, printer_publisher_guilds).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, vernacular_literate_laity).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, catholic_ecclesiastical_censors).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, displaced_scribal_copyists).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, unlicensed_pamphlet_printers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, unlicensed_pamphlet_printers).
narrative_ontology:constraint_vindicates(press_reformation_causation__mutual_shaping, co_constitution_of_technology_and_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reformers like Luther and his allies did not merely receive the press as a finished tool; they iteratively pushed printers toward cheaper formats, shorter pamphlets, and vernacular typesetting conventions that made rapid, wide, repeatable distribution possible. Their theological output and the press's material development shaped each other over the interval; they had multiple regional printers to work with and could relocate production when one city's authorities cracked down.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, protestant_reform_networks, beneficiary,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__mutual_shaping, protestant_reform_networks, agenda_setter).

% Printers in Wittenberg, Strasbourg, Basel, and Antwerp discovered that reform pamphlets were commercially superior to Latin devotional works — cheaper to set, faster to sell, reprintable. They adapted typefaces, formats, and print runs to this demand, and that adaptation in turn expanded what reformers could attempt next (shorter tracts, woodcut broadsides, serialized disputations). Guild members could move workshops between jurisdictions to escape licensing crackdowns.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, printer_publisher_guilds, beneficiary,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__mutual_shaping, printer_publisher_guilds, agenda_setter).

% Newly literate or semi-literate townspeople gained access to vernacular scripture and polemic they could read or have read aloud to them, and their appetite for this material fed back into what printers commissioned and reformers wrote. Their exit is constrained by literacy, geography, and local availability of print shops rather than by any single authority's decree.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, vernacular_literate_laity, beneficiary,
    moderate, biographical, constrained, regional).

% Church authorities attempted licensing regimes, indices of prohibited books, and local printing bans, but the press's decentralized, mobile, capital-light workshop model meant enforcement in one jurisdiction simply shifted production elsewhere. They bore the cost of a rapidly proliferating adversarial print culture they could administer against but not structurally suppress, and their own use of print for counter-polemic reshaped how they engaged the technology.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, catholic_ecclesiastical_censors, payer,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__mutual_shaping, catholic_ecclesiastical_censors, agenda_setter).

% Manuscript copyists whose livelihood depended on hand-reproduction of texts lost their economic niche as movable-type printing scaled; they had no meaningful path to retrain into or compete with the new production regime, which was itself being reshaped continuously by reformer demand for volume and speed.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, displaced_scribal_copyists, payer,
    powerless, biographical, trapped, regional).

% Small operators who printed unauthorized reform tracts profited when demand was high but bore confiscation, fines, and occasional imprisonment when local authorities enforced licensing; their willingness to keep printing under this risk was itself part of what pushed press technology and distribution networks toward resilience against censorship.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, unlicensed_pamphlet_printers, payer,
    moderate, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__mutual_shaping, unlicensed_pamphlet_printers, beneficiary).

% Retrospective analysts assessing whether the press determined the Reformation, was merely a neutral tool reformers deployed, or co-evolved with reformist practice in a bidirectional loop. This story's authoring seat is committed to the third reading.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, historians_of_technology, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causation__mutual_shaping, diffuse).
narrative_ontology:fixing_cost_class(press_reformation_causation__mutual_shaping, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The press-and-reform assemblage solved a genuine distribution problem: getting theological argument, vernacular scripture, and polemic to dispersed, semi-literate populations faster and cheaper than manuscript copying or oral transmission allowed, while iteratively adapting print technology itself to that distribution demand.
% TRANSFER_FUNCTION: Moves religious authority and interpretive control away from centralized ecclesiastical licensing and manuscript scriptoria toward decentralized printer-reformer networks and their lay readerships; moves economic livelihood away from scribal copyists and toward printer-publisher guilds; moves enforcement burden onto ecclesiastical censors who must now contest a self-reinforcing technology-practice loop rather than a fixed, controllable production chain.
% ABSENT_VOICES: Scribal copyists left almost no organized documentary voice objecting to their displacement — their trade had no guild capable of contesting the shift the way ecclesiastical authorities could contest doctrine. Illiterate rural populations, largely untouched by either side of the print contest, are absent from a story told almost entirely through literate urban records.
% DISAPPEARANCE_RATIONALE: If the mutually-reinforcing press-reform loop had not existed — if either the press had remained a purely Latin devotional technology or reformers had lacked a technology responsive to their distribution needs — reform ideas would have propagated far more slowly through manuscript and sermon alone, ecclesiastical licensing would likely have remained an effective control mechanism, and the printing trade itself would have developed along different formal and economic lines (larger, more centralized, slower-turnover shops rather than the decentralized pamphlet economy that actually emerged).
% FOUNDING_PROBLEM: Neither side set out to build 'the mutual-shaping loop' — reformers needed faster, cheaper distribution for contested theological claims that manuscript copying and preaching alone could not achieve at scale, and printers needed reliable high-volume demand to make movable-type capital investment profitable. The co-evolutionary loop emerged from these two independent problems intersecting repeatedly over decades.
% FOUNDING_PROBLEM_CORROBORATION: Book historians (e.g., quantitative print-run studies of Wittenberg and Strasbourg presses) attest, independent of confessional allegiance, that the original distribution-and-capital problems were resolved by the mid-to-late 16th century as print became the default publishing technology across confessions; the co-evolutionary dynamic itself is now studied as a historical episode rather than a live arrangement anyone maintains. No living party 'benefits' from the founding problem remaining unsolved, which is consistent with the loop having genuinely closed rather than persisting as extraction.
narrative_ontology:disappearance_verdict(press_reformation_causation__mutual_shaping, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__mutual_shaping, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__mutual_shaping, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(press_reformation_causation__mutual_shaping, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__mutual_shaping, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__mutual_shaping_tests).
:- end_tests(press_reformation_causation__mutual_shaping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises moderately (0.22 to 0.38) over the interval as the printer-reformer assemblage matured from experimental pamphleteering into an established distribution-and-revenue system that displaced scribal labor and imposed real costs on censorial institutions; it plateaus rather than continuing to climb because by the 1550s the co-evolutionary loop had largely stabilized into settled trade and confessional print practices rather than continuing to intensify. Suppression tracks the same curve because active enforcement (licensing regimes, book bans, confiscation) intensified precisely as the loop became harder to contain, then plateaued as authorities adapted rather than eliminated the practice. Theater ratio stays low throughout — the enforcement and production activity were substantially functional, not performative, though a growing minority of licensing gestures (nominal approvals, symbolic condemnations that did not stop actual printing) account for the modest rise to 0.22.
 *
 * PERSPECTIVAL GAP:
 *   From the reformer-and-printer seat, this looks like productive coordination: a genuine solution to a genuine distribution problem, with real winners and no coercion required to sustain participation. From the ecclesiastical-censor seat, the same structure looks like an escalating enforcement burden against a technology-practice loop that could not be brought back under centralized control. From the scribal-copyist seat, it looks like pure displacement with no coordination benefit at all. The engine computing three different per-seat readings from one set of structural facts is exactly the point of the mutual-shaping frame: the causation runs both ways, so the extraction and the coordination are genuinely co-located in the same structure rather than separable into a 'good' technology and a 'bad' use of it.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformers and printers sit near the beneficiary end (d low): they set terms, could relocate operations, and captured the surplus of an expanding market. Vernacular laity sit closer to symmetric — real benefit from access, but no control over supply or terms. Ecclesiastical censors and unlicensed printers who got caught sit toward the target end for different reasons: censors bear the institutional cost of an increasingly uncontrollable technology-practice loop, while small unlicensed printers bear direct legal risk for participating in the very innovation that made the loop resilient. Scribal copyists sit at the full-target end: trapped, powerless, with no coordination benefit whatsoever — they are casualties of the co-evolution, not participants in it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (distribution bottleneck plus capital-investment risk) is dead by the mid-16th century — print was fully normalized as the dominant textual technology across confessions. This is not a mandatrophy case in the extraction-persists-after-function-dies sense: the coordination function did not calcify into rent-seeking after resolving its founding problem; it simply completed its transitional arc and gave way to an ordinary, non-transitional print economy. The has_sunset_clause and scaffold claim reflect that this was a scaffold that did sunset rather than an arrangement whose administrators had incentive to prolong it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location_of_disagreement,
    'Where exactly do the three readings of the press_reformation_causation kernel locate their disagreement — is it about the DIRECTION of causal priority (press-first vs. agency-first vs. bidirectional), or about the DEGREE of technological constraint on possible reformist strategies?',
    'Comparative historical analysis of counterfactual cases: regions/periods with press technology but no reform movement, and reform-adjacent movements (e.g., earlier Hussite or Wycliffite reform) that lacked print, would help isolate whether press affordances or reformist agency did more independent causal work, versus whether the two were genuinely inseparable.',
    'If evidence strongly favors one direction (press availability alone predicting reform success, or reform intensity alone predicting press adaptation regardless of prior press infrastructure), this mutual_shaping reading would be undermined in favor of one of its siblings; the current story''s scaffold classification depends on the bidirectional reading holding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location_of_disagreement, conceptual, 'Whether the kernel''s contested readings differ on causal direction or causal degree, and what evidence would discriminate.').

omega_variable(
    co_evolution_vs_convenient_synthesis,
    'Is ''mutual shaping'' a genuine structural finding, or is it the intellectually comfortable synthesis position that avoids committing to either determinism or pure agency without independent evidential support of its own?',
    'Look for specific, dated instances where a press technical innovation (e.g., a typeface, a format, a distribution route) demonstrably arose FROM reformist demand AND subsequently enabled a reformist tactic that would not have been feasible before that innovation — a genuine feedback loop requires evidence of both directions of the arrow, not just co-occurrence.',
    'If such concrete bidirectional feedback instances are sparse, this reading may be under-evidenced relative to its rhetorical appeal, and the classification here (scaffold with real coordination function) would be less secure than the technological_determinism or strategic_deployment readings for the same historical record.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(co_evolution_vs_convenient_synthesis, conceptual, 'Whether the mutual-shaping reading is well-evidenced or is a default synthesis position.').

omega_variable(
    scribal_displacement_naturalness,
    'Is the displacement of scribal copyists better understood as an unavoidable technological transition (a mountain-like cost of progress) or as a constructed extraction that the co-evolving press-reform assemblage could have mitigated but did not?',
    'Comparative study of print transitions in other regions/periods where scribal guilds had more organized political power (e.g., some Islamic manuscript traditions that resisted print adoption for centuries) to see whether scribal displacement severity correlates with scribal organizational power rather than with print technology''s inherent properties.',
    'If displacement severity tracks scribal political power rather than technological necessity, the victim classification for scribal_copyists understates a constructed (not merely natural) extraction, and suppression may be under-measured for that seat specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scribal_displacement_naturalness, empirical, 'Whether scribal displacement was technologically inevitable or contingent on relative political power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__mutual_shaping, 1517, 1555).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1517, press_reformation_causation__mutual_shaping, theater_ratio, 1517, 0.1).
narrative_ontology:measurement(pres_tr_t1522, press_reformation_causation__mutual_shaping, theater_ratio, 1522, 0.13).
narrative_ontology:measurement(pres_tr_t1530, press_reformation_causation__mutual_shaping, theater_ratio, 1530, 0.17).
narrative_ontology:measurement(pres_tr_t1540, press_reformation_causation__mutual_shaping, theater_ratio, 1540, 0.2).
narrative_ontology:measurement(pres_tr_t1548, press_reformation_causation__mutual_shaping, theater_ratio, 1548, 0.21).
narrative_ontology:measurement(pres_tr_t1555, press_reformation_causation__mutual_shaping, theater_ratio, 1555, 0.22).

% Extraction over time
narrative_ontology:measurement(pres_be_t1517, press_reformation_causation__mutual_shaping, base_extractiveness, 1517, 0.22).
narrative_ontology:measurement(pres_be_t1522, press_reformation_causation__mutual_shaping, base_extractiveness, 1522, 0.28).
narrative_ontology:measurement(pres_be_t1530, press_reformation_causation__mutual_shaping, base_extractiveness, 1530, 0.34).
narrative_ontology:measurement(pres_be_t1540, press_reformation_causation__mutual_shaping, base_extractiveness, 1540, 0.37).
narrative_ontology:measurement(pres_be_t1548, press_reformation_causation__mutual_shaping, base_extractiveness, 1548, 0.38).
narrative_ontology:measurement(pres_be_t1555, press_reformation_causation__mutual_shaping, base_extractiveness, 1555, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1517, press_reformation_causation__mutual_shaping, suppression_requirement, 1517, 0.2).
narrative_ontology:measurement(pres_su_t1522, press_reformation_causation__mutual_shaping, suppression_requirement, 1522, 0.3).
narrative_ontology:measurement(pres_su_t1530, press_reformation_causation__mutual_shaping, suppression_requirement, 1530, 0.38).
narrative_ontology:measurement(pres_su_t1540, press_reformation_causation__mutual_shaping, suppression_requirement, 1540, 0.41).
narrative_ontology:measurement(pres_su_t1548, press_reformation_causation__mutual_shaping, suppression_requirement, 1548, 0.42).
narrative_ontology:measurement(pres_su_t1555, press_reformation_causation__mutual_shaping, suppression_requirement, 1555, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__mutual_shaping, information_standard).
narrative_ontology:boltzmann_floor_override(press_reformation_causation__mutual_shaping, 0.05).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, strategic_deployment).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the press_reformation_causation kernel. technological_determinism authors the press's material affordances as the dominant independent variable (near-mountain framing, low agency-attribution). strategic_deployment authors reformist and printer agency as the dominant independent variable (near-rope framing, technology as neutral instrument). This mutual_shaping story authors bidirectional co-construction (scaffold framing: a jointly-built transitional structure with real coordination function and real asymmetric costs). Each carries its own ε, its own claimed_type, and its own stakeholder structure; they are linked here rather than merged because the kernel's referent (what actually happened between 1517 and 1555) is shared even though each reading's causal-structure claim, and therefore its classification, differs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
