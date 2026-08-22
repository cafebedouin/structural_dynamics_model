% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__mutual_shaping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Press-Reformation Mutual Shaping (Bidirectional Causation Reading)
 *   domain: history/technology/religious
 *
 * SUMMARY:
 *   Between 1450 and 1550, the printing press and the Protestant Reformation
 *   co-evolved through bidirectional causation: technology enabled reformers'
 *   ambitions, which reformers then realized through sustained demand for
 *   printer innovation, which in turn transformed what printing could do.
 *   This reading rejects pure technological determinism (the press made the
 *   Reformation inevitable) and pure strategic deployment (reformers simply
 *   used a neutral tool). Instead, reformers and printers mutually shaped the
 *   press's trajectory—its cost structure, vernacular orientation,
 *   portability, and speed—while that very evolution enabled the religious
 *   and political transformations that would not have occurred otherwise. The
 *   constraint is a scaffold: a temporary enabling structure whose
 *   justification is the transition it enables (from ecclesiastical monopoly
 *   on scripture to distributed lay reading and reformed theology). The
 *   mutual shaping reading is one of three competing readings of the
 *   press-Reformation kernel.
 *
 * KEY AGENTS:
 *   - Reformation reformers: organize demand for vernacular, rapid-reproduction texts, shaping printer investment and technical focus
 *   - Printing technicians and entrepreneurs: innovate in response to reformer demand, develop cheaper/faster capabilities, relocate to reformist jurisdictions
 *   - Ecclesiastical authority: faces displacement of gatekeeping functions, incurs rising suppression costs
 *   - Secular rulers: navigate between Church pressure and press-driven economic/informational expansion
 *   - Lay reading publics: benefit from access to texts previously mediated by clergy; their growing literacy and demand reinforce printer innovation
 *   - Scribal networks: structurally displaced by press-driven economics; some adapt by entering the printing industry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__mutual_shaping, 0.38).
domain_priors:suppression_score(press_reformation_causation__mutual_shaping, 0.45).
domain_priors:theater_ratio(press_reformation_causation__mutual_shaping, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, extractiveness, 0.38).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__mutual_shaping, scaffold).
narrative_ontology:human_readable(press_reformation_causation__mutual_shaping, "Press-Reformation Mutual Shaping (Bidirectional Causation Reading)").
narrative_ontology:topic_domain(press_reformation_causation__mutual_shaping, "history/technology/religious").

narrative_ontology:has_sunset_clause(press_reformation_causation__mutual_shaping).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__mutual_shaping, '4a5a7b76-edb7-4e56-bd35-e1b1b5673006').
narrative_ontology:cs_kernel_codification('4a5a7b76-edb7-4e56-bd35-e1b1b5673006', distributed).
narrative_ontology:cs_authority_grounding('4a5a7b76-edb7-4e56-bd35-e1b1b5673006', expertise).
narrative_ontology:cs_reading_relation('4a5a7b76-edb7-4e56-bd35-e1b1b5673006', press_reformation_causation__technological_determinism, coexists_with).
narrative_ontology:cs_reading_relation('4a5a7b76-edb7-4e56-bd35-e1b1b5673006', press_reformation_causation__strategic_deployment, coexists_with).
narrative_ontology:cs_axiom('4a5a7b76-edb7-4e56-bd35-e1b1b5673006', foundational, technology_agency_codependence).
narrative_ontology:cs_axiom_status(technology_agency_codependence, holdable).
narrative_ontology:cs_axiom_grounding('4a5a7b76-edb7-4e56-bd35-e1b1b5673006', technology_agency_codependence, empirically_contingent).
narrative_ontology:cs_axiom('4a5a7b76-edb7-4e56-bd35-e1b1b5673006', secondary, neither_technology_nor_agency_primary).
narrative_ontology:cs_axiom_status(neither_technology_nor_agency_primary, holdable).
narrative_ontology:cs_axiom_grounding('4a5a7b76-edb7-4e56-bd35-e1b1b5673006', neither_technology_nor_agency_primary, instrumental).
narrative_ontology:cs_reference_frame('4a5a7b76-edb7-4e56-bd35-e1b1b5673006', manuscript_monopoly_ecclesiastical_authority).
narrative_ontology:cs_drift_state('4a5a7b76-edb7-4e56-bd35-e1b1b5673006', distributed_lay_reading_reformed_theology, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4a5a7b76-edb7-4e56-bd35-e1b1b5673006', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__mutual_shaping, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, reformation_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, vernacular_scripture_advocates).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, printing_technicians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, secular_rulers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, lay_reading_public).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, ecclesiastical_authority).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, secular_rulers).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, scribal_manuscript_networks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Religious and political figures seeking to challenge ecclesiastical authority and distribute scripture in vernacular languages. They recognize and exploit the press's capacity to reach audiences at scale, but this capacity itself emerges from their demands for rapid, cheap reproduction of controversial texts. They shape printing development through their consistent orders for reformist materials.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, reformation_reformers, beneficiary,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__mutual_shaping, reformation_reformers, agenda_setter).

% Craftspeople and entrepreneurs operating printing shops. They respond to market demand from reformers by developing techniques, improving speed and affordability, and relocating to jurisdictions where reformist texts can be printed and sold. Their innovations (cheaper paper sourcing, faster press configurations, portable equipment) are driven by reformer demand but become general-purpose capabilities that reshape what printing can do.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, printing_technicians, beneficiary,
    moderate, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__mutual_shaping, printing_technicians, agenda_setter).

% Church hierarchy faces an unprecedented loss of control over scriptural interpretation and doctrine dissemination. The press's capacity to reproduce texts faster than ecclesiastical scribal networks and in languages the Church did not monopolize shifts the authority structure. Suppression (censorship, book banning, persecution of printers) becomes necessary to maintain doctrinal gatekeeping, but the cost of suppression rises as print technology becomes embedded in commerce and literacy.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, ecclesiastical_authority, payer,
    institutional, generational, trapped, continental).

% Political authorities navigate between the Church's pressure to ban reformist materials and the growing economic and informational power of printing technology. Some exploit the press for their own state-building (vernacular legal codes, administrative records); others suppress it under Church influence. Their exit options are limited—they cannot simply stop printing's development, only channel or obstruct it.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, secular_rulers, payer,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__mutual_shaping, secular_rulers, beneficiary).

% Populations with growing literacy (urban, merchant-class, artisanal) gain access to scripture, news, and knowledge previously mediated by clergy. The press creates the possibility; reformer demand for distribution creates the economic case; printer innovation makes the price accessible. Their access expands the political and religious consciousness that in turn drives further demand for reformist and commercial texts.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, lay_reading_public, beneficiary,
    powerless, biographical, constrained, regional).

% Monastic and university scriptoria that produced hand-copied manuscripts face displacement as printing becomes the standard medium for book production. Their decline is both cause and effect: reformers' demand for rapid reproduction makes printing preferable, and printing's success makes the scribal model economically uncompetitive. Some scribal networks adapt by entering the printing industry; others disappear.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, scribal_manuscript_networks, payer,
    moderate, biographical, constrained, continental).

% Scholarly analysts examining the causal relationship between technology and social change. They observe that neither the press alone nor reformer agency alone explains the Reformation's spread; instead, the two co-evolved—each reshaping the other's possibilities and constraints across a century.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, technology_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causation__mutual_shaping, diffuse).
narrative_ontology:fixing_cost_class(press_reformation_causation__mutual_shaping, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The printing press solves a coordination problem for reformers: how to reach geographically dispersed audiences with identical texts at a cost below what scribal networks could offer. It solves a coordination problem for printers: how to invest in equipment when demand is uncertain. These problems are genuinely solved by the technology, but the technology's particular trajectory—toward cheapness, vernacularity, portability—is shaped by the specific demands of reformers, not by the technology's inherent logic.
% TRANSFER_FUNCTION: Transfers scriptural authority from ecclesiastical gatekeepers to a distributed network of readers, printers, and reformers. Transfers economic value from scribal labor to print technicians and entrepreneurs. Transfers the cost of suppression from Church resources onto secular rulers who must choose between Church pressure and economic policy.
% ABSENT_VOICES: Non-literate populations have no seat at the table; the constraint privileges readers and those with access to press products. Manuscript craftspeople are structurally excluded as their labor becomes economically obsolete. Printers who did not specialize in reformist materials have constrained voice—the constraint shapes printing toward religious and political controversy, not toward other potential uses (scientific texts, commercial records) that emerged more slowly.
% DISAPPEARANCE_RATIONALE: If the mutual-shaping reading vanishes and we return to pure technological determinism or pure strategic-deployment framings, the historical narrative changes fundamentally. But if the material constraint—the press itself, in its historically-evolved form—disappears, scriptural dissemination reverts to manuscript networks, ecclesiastical control over doctrine reasserts, and political authority consolidates around monopolized literacy. The Reformation as it happened does not occur without this specific technology-agency co-evolution.
% FOUNDING_PROBLEM: Religious reformers seek to distribute vernacular scripture and challenge Church authority at continental scale. Printing technicians seek profitable markets for expanding press capacity. These demands encounter the existing constraint: manuscript reproduction cannot meet the volume or speed demanded. The constraint that solves these problems is not yet fully formed—it emerges through iterative interaction between reformer demand and printer innovation.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Reformation (A.G. Dickens, Andrew Pettegree, Alec Ryrie) document that reformers explicitly recognized and exploited printing's capacity for rapid reproduction and that printer records show intentional innovation in response to reformist demand. Historians of technology (Elizabeth Eisenstein, Ursula Klein, Adrian Johns) document the bidirectional shaping of printing press development. Independent corroboration comes from the technology history literature treating printing as a co-evolved system, not a determined cause.
narrative_ontology:disappearance_verdict(press_reformation_causation__mutual_shaping, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__mutual_shaping, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__mutual_shaping, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(press_reformation_causation__mutual_shaping, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__mutual_shaping, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is low-to-moderate (0.38 at interval end) because the constraint redistributes authority and access rather than concentrating benefits in one seat. Ecclesiastical authority loses gatekeeping; lay readers gain access; printers gain markets; reformers gain efficacy. The net effect is diffuse benefit accrual rather than concentrated extraction. Suppression rises over the interval (from 0.1 to 0.45) as the Church recognizes the threat and attempts censorship and persecution, but suppression plateaus because printers' mobility and the economic embedding of printing technology limit the Church's enforcement capacity. Theater ratio is low (0.22) because the constraint's primary function—enabling rapid reproduction at scale—remains operative throughout the interval; the theatrical elements (religious justification for technical innovation, performative suppression) are secondary. Accessibility collapse is moderate (0.62): alternatives to printing exist (manuscript, oral transmission) but become progressively less viable as printing becomes the standard medium. Resistance is high (0.71) because ecclesiastical authority actively opposes the constraint's operation and printers actively evade suppression; this is not a quiet, accepted arrangement. The measurement series traces extractiveness rising steeply through the first two decades, then plateauing as the co-evolutionary dynamic stabilizes; suppression follows a similar trajectory, peaking around 1530 as the Church's suppression machinery reaches maximum intensity, then declining slightly as enforcement attrition sets in. Theater ratio rises gradually as the constraint's operation becomes institutionalized and less nakedly contentious.
 *
 * PERSPECTIVAL GAP:
 *   The mutual-shaping reading asserts that neither reformer agency nor press capability alone explains the outcome; both are necessary and mutually constitutive. A reformer-seat analysis emphasizes their strategic intelligence and intentional exploitation of the press; a printer-seat analysis emphasizes their technical creativity and profit motive; an ecclesiastical-seat analysis emphasizes their involuntary displacement and rising suppression burden; a historian-seat analysis emphasizes the co-evolutionary dynamics that produced neither as inevitable. These are not contradictions; they are facets of a constraint whose operation produces different experienced types at different seats. The engine computes per-seat classifications from the structural data (beneficiary/victim declarations, power, exit options); the author declares the structural asymmetry and lets the engine reveal what each seat computes as.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformers occupy a beneficiary-and-agenda-setter position (organized power, capable of driving demand that shapes printer behavior). Printers occupy a beneficiary position but also a constrained agenda-setter role (they respond to demand but their innovations reshape what demand is possible). Ecclesiastical authority is the primary payer (loses gatekeeping, incurs suppression costs). Secular rulers sit in a mixed position: they benefit from press-driven literacy and economic activity but are constrained by Church pressure to suppress. Lay readers are beneficiaries (gain access) but powerless (lack direct agency in shaping the constraint). The directionality divergence should be sharp: from the reformer seat, the press is a tool they exploit and refine (low d, beneficiary framing). From the ecclesiastical seat, the press is an imposed constraint that extracts authority regardless of their preferences (high d, target framing). From the printer seat, the press is a demand-responsive opportunity (moderate d, moving toward beneficiary as demand intensifies). This divergence is the seat-level type computation the engine should reveal; the author does not adjudicate it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (religious authority monopoly on scriptural interpretation) is live at the interval's start and contested by its end—reformers and their lay allies assert it is solved; ecclesiastical authority asserts it persists (or should persist). By 1550, the problem is materially weakened but not eliminated: ecclesiastical authority still functions in Catholic regions and among non-reading populations. The constraint is claimed as a scaffold because its justification is transition: it enables the shift from monopoly authority to distributed lay reading and reformed theology. The sunset clause is implicit in the founding problem's status—once scriptural authority is genuinely distributed and ecclesiastical gatekeeping becomes unenforceable (by century's end), the constraint has completed its transitional function. However, the material printing technology persists; what ends is the specifically-mutual-shaping dynamic of reformer demand pulling printer innovation forward in a tight co-evolutionary loop. By 1550, printing is an established infrastructure; the scaffolding role (enabling transformation) transitions to infrastructure role (supporting the transformed arrangement). This is the classic scaffold lifecycle: temporary enabling structure that becomes permanent infrastructure, at which point it is no longer a scaffold but a rope or mountain (depending on whether its continuation requires active coordination or emerges from economic necessity).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causality_direction_ambiguity,
    'Which causal direction is primary: did reformer demand drive printer innovation (reformers→technology), or did printing''s emerging capabilities enable reformer ambitions (technology→reformers), or is the causality genuinely bidirectional with neither primary?',
    'Chronological analysis of printer technical innovations and reformer organizational strategies. Identifying innovations that were not demanded and reformer strategies that could not have been executed without those innovations. Counterfactual analysis: what would reformism look like without the press; what would printing look like without reformer demand.',
    'A unidirectional finding would support either technological_determinism or strategic_deployment readings; bidirectional confirmation would support mutual_shaping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causality_direction_ambiguity, empirical, 'The direction and symmetry of causal influence between technology and agency.').

omega_variable(
    scaffold_vs_infrastructure_boundary,
    'At what point does the press transition from scaffold (enabling structure for transformation) to infrastructure (supporting structure for the transformed arrangement)? Is there a discrete moment or a gradual drift?',
    'Historical analysis of when printing becomes economically self-sustaining independent of reformist demand; when suppression costs to the Church become prohibitive; when non-religious printing (administrative, commercial, scientific) becomes the dominant use case.',
    'If the transition is discrete, the constraint''s type classification might shift from scaffold to rope or piton at a specific historical moment; if gradual, the type should reflect the transitional state across a longer interval.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scaffold_vs_infrastructure_boundary, conceptual, 'The lifecycle boundary between enabling and supporting functions.').

omega_variable(
    reading_contest_kernel_identity,
    'Is the contested kernel the causal relationship between technology and Reformation, or is it the more fundamental claim about whether technology and agency are codependent or independent ontologically?',
    'Analytical: does accepting the mutual_shaping reading commit one to a general thesis about technology-agency codependence, or only to the specific press-Reformation case? Are there domains where causality is unidirectional (technology-determined or purely strategic)?',
    'If codependence is general, the reading''s scope extends beyond the Reformation; if case-specific, the mutual_shaping reading does not adjudicate the general question and sibling readings might hold in other domains.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_kernel_identity, conceptual, 'The scope of the reading''s ontological commitments.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the Church''s suppression of printing and reformist texts structurally effective in limiting dissemination, or is it merely performative—appearing to enforce orthodoxy while failing to prevent distribution at scale?',
    'Quantitative analysis of banned vs. circulating texts; geographic correlation between suppression intensity and reformer/printer presence; comparison of suppression cost to printing revenue in different jurisdictions.',
    'Performative suppression would suggest the constraint''s persistence depends more on inertia and threat than on actual extraction; effective suppression would suggest the Church''s enforcement capacity is higher than the authored metrics indicate. This affects whether the constraint is best classified as snare (effective extraction) or piton (theatrical maintenance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether Church suppression is structurally effective or primarily theatrical.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__mutual_shaping, 1450, 1550).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causation__mutual_shaping, theater_ratio, 1450, 0.08).
narrative_ontology:measurement_basis(pres_tr_t1450, projected).
narrative_ontology:measurement(pres_tr_t1480, press_reformation_causation__mutual_shaping, theater_ratio, 1480, 0.12).
narrative_ontology:measurement_basis(pres_tr_t1480, observed).
narrative_ontology:measurement(pres_tr_t1510, press_reformation_causation__mutual_shaping, theater_ratio, 1510, 0.18).
narrative_ontology:measurement_basis(pres_tr_t1510, observed).
narrative_ontology:measurement(pres_tr_t1530, press_reformation_causation__mutual_shaping, theater_ratio, 1530, 0.22).
narrative_ontology:measurement_basis(pres_tr_t1530, observed).
narrative_ontology:measurement(pres_tr_t1550, press_reformation_causation__mutual_shaping, theater_ratio, 1550, 0.22).
narrative_ontology:measurement_basis(pres_tr_t1550, observed).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causation__mutual_shaping, base_extractiveness, 1450, 0.12).
narrative_ontology:measurement_basis(pres_be_t1450, projected).
narrative_ontology:measurement(pres_be_t1480, press_reformation_causation__mutual_shaping, base_extractiveness, 1480, 0.22).
narrative_ontology:measurement_basis(pres_be_t1480, observed).
narrative_ontology:measurement(pres_be_t1510, press_reformation_causation__mutual_shaping, base_extractiveness, 1510, 0.35).
narrative_ontology:measurement_basis(pres_be_t1510, observed).
narrative_ontology:measurement(pres_be_t1530, press_reformation_causation__mutual_shaping, base_extractiveness, 1530, 0.38).
narrative_ontology:measurement_basis(pres_be_t1530, observed).
narrative_ontology:measurement(pres_be_t1550, press_reformation_causation__mutual_shaping, base_extractiveness, 1550, 0.38).
narrative_ontology:measurement_basis(pres_be_t1550, observed).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1450, press_reformation_causation__mutual_shaping, suppression_requirement, 1450, 0.1).
narrative_ontology:measurement_basis(pres_su_t1450, projected).
narrative_ontology:measurement(pres_su_t1480, press_reformation_causation__mutual_shaping, suppression_requirement, 1480, 0.28).
narrative_ontology:measurement_basis(pres_su_t1480, observed).
narrative_ontology:measurement(pres_su_t1510, press_reformation_causation__mutual_shaping, suppression_requirement, 1510, 0.42).
narrative_ontology:measurement_basis(pres_su_t1510, observed).
narrative_ontology:measurement(pres_su_t1530, press_reformation_causation__mutual_shaping, suppression_requirement, 1530, 0.48).
narrative_ontology:measurement_basis(pres_su_t1530, observed).
narrative_ontology:measurement(pres_su_t1550, press_reformation_causation__mutual_shaping, suppression_requirement, 1550, 0.45).
narrative_ontology:measurement_basis(pres_su_t1550, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__mutual_shaping, global_infrastructure).
narrative_ontology:boltzmann_floor_override(press_reformation_causation__mutual_shaping, 0.25).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, press_reformation_causation__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, press_reformation_causation__strategic_deployment).

% DUAL FORMULATION NOTE:
% The press-Reformation causal relationship is the subject of three structurally distinct constraint readings in the press_reformation_causation kernel: technological_determinism (the press caused the Reformation by making censorship impossible), strategic_deployment (reformers strategically exploited the press as a neutral tool), and mutual_shaping (technology and agency co-evolved; this story). Each reading instantiates a different constraint with different beneficiary structures, extraction profiles, and type classifications. The mutual_shaping reading rejects the primacy assumption of both sibling readings and asserts codependence. All three stories should be generated and linked via affects_constraints to form the kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
