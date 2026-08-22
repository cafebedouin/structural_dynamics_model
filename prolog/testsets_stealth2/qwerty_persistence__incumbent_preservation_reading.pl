% ============================================================================
% CONSTRAINT STORY: qwerty_persistence__incumbent_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence__incumbent_preservation_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: qwerty_persistence__incumbent_preservation_reading
 *   human_readable: QWERTY Layout Persistence via Incumbent Defense (Incumbent Preservation Reading)
 *   domain: technological/historical
 *
 * SUMMARY:
 *   This story instantiates the incumbent_preservation_reading of the
 *   qwerty_persistence kernel: the QWERTY layout persists not merely because
 *   coordination favors it, but because parties with capital committed to it
 *   — manufacturers with tooling and installed base, typists with
 *   layout-specific human capital, training institutions with curricula and
 *   certifications — actively defend it, and that defense imposes real costs
 *   on would-be adopters of alternatives and on employers seeking efficiency.
 *   The constraint is claimed as tangled_rope: a genuine skill-portability
 *   coordination function wrapped around an asymmetric extraction structure
 *   maintained by defaults, codification, and training pipelines. Per the
 *   epsilon-invariance decomposition rule, the colloquial label 'QWERTY
 *   persistence' splits into two constraint stories: this one
 *   (defense-sustained persistence; epsilon includes defensive suppression
 *   expenditure and imposed switching friction; victim set present) and the
 *   sibling qwerty_persistence__lapsed_alternatives_reading
 *   (coordination-value persistence; alternatives lapse for want of critical
 *   mass; lower epsilon, no victim set). The two differ in epsilon
 *   composition, not in the underlying persistence facts, and are linked
 *   through network.affects_constraints. The referent of epsilon here is the
 *   standing QWERTY arrangement as this reading assesses it — not the
 *   alternative-layout regime this reading's critics would prefer. Claim and
 *   metrics are authored independently: tangled_rope is what I believe
 *   structurally true; the metric values are what I believe descriptively
 *   true of the arrangement's operation.
 *
 * KEY AGENTS:
 *   - - incumbent_keyboard_manufacturers: agenda setter (institutional/arbitrage) — sets the hardware default on every shipped device; tooling, legends, and firmware committed to QWERTY; historically competed rival layouts out of the market
 *   - - trained_typists: primary beneficiary (moderate/constrained) — QWERTY-specific muscle memory is their portable professional asset; switching means months of degraded output
 *   - - typing_training_institutions: beneficiary (organized/constrained) — sell QWERTY instruction; curricula, textbooks, and accreditation benchmarks denominated in the incumbent layout
 *   - - efficiency_seeking_employers: payer with a genuine secondary beneficiary position (organized/constrained) — want throughput above the incumbent ceiling but cannot run a mixed-layout office
 *   - - alternative_layout_adopters: payer (moderate/constrained) — Dvorak/Colemak users bearing a standing compatibility levy on shared machines, hiring tests, and support
 *   - - alternative_layout_designers: excluded (moderate/trapped) — layout designers with no seat in the standardization process their proposals must pass through
 *   - - standards_bodies: agenda setter (institutional/analytical) — codify the incumbent layout as the national/international standard without independent efficiency trials of alternatives
 *   - - economic_historians: analytical observer — the seat from which the kernel's rival readings are articulated and tested
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__incumbent_preservation_reading, 0.48).
domain_priors:suppression_score(qwerty_persistence__incumbent_preservation_reading, 0.55).
domain_priors:theater_ratio(qwerty_persistence__incumbent_preservation_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__incumbent_preservation_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence__incumbent_preservation_reading, "QWERTY Layout Persistence via Incumbent Defense (Incumbent Preservation Reading)").
narrative_ontology:topic_domain(qwerty_persistence__incumbent_preservation_reading, "technological/historical").

domain_priors:requires_active_enforcement(qwerty_persistence__incumbent_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__incumbent_preservation_reading, '6384c537-6154-47d0-a12c-ddb96d1a14e7').
narrative_ontology:cs_kernel_codification('6384c537-6154-47d0-a12c-ddb96d1a14e7', formalized).
narrative_ontology:cs_authority_grounding('6384c537-6154-47d0-a12c-ddb96d1a14e7', extraction).
narrative_ontology:cs_interpretation_layer_present('6384c537-6154-47d0-a12c-ddb96d1a14e7').
narrative_ontology:cs_reading_relation('6384c537-6154-47d0-a12c-ddb96d1a14e7', qwerty_persistence__lapsed_alternatives_reading, coexists_with).
narrative_ontology:cs_axiom('6384c537-6154-47d0-a12c-ddb96d1a14e7', foundational, persistence_requires_active_defense).
narrative_ontology:cs_axiom_status(persistence_requires_active_defense, holdable).
narrative_ontology:cs_axiom_grounding('6384c537-6154-47d0-a12c-ddb96d1a14e7', persistence_requires_active_defense, empirically_contingent).
narrative_ontology:cs_axiom('6384c537-6154-47d0-a12c-ddb96d1a14e7', secondary, incumbent_capital_defense_is_extractive).
narrative_ontology:cs_axiom_status(incumbent_capital_defense_is_extractive, holdable).
narrative_ontology:cs_axiom_grounding('6384c537-6154-47d0-a12c-ddb96d1a14e7', incumbent_capital_defense_is_extractive, empirically_contingent).
narrative_ontology:cs_reference_frame('6384c537-6154-47d0-a12c-ddb96d1a14e7', actively_defended_incumbency).
narrative_ontology:cs_drift_state('6384c537-6154-47d0-a12c-ddb96d1a14e7', contemporary_software_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6384c537-6154-47d0-a12c-ddb96d1a14e7', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__incumbent_preservation_reading, qwerty_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, incumbent_keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, trained_typists).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, typing_training_institutions).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, alternative_layout_adopters).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, efficiency_seeking_employers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, efficiency_seeking_employers).
narrative_ontology:constraint_vindicates(qwerty_persistence__incumbent_preservation_reading, path_dependence_economics_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and build keyboards and set the factory default layout on every device shipped; their tooling, keycap legends, firmware, and installed base are committed to the incumbent layout. Historically (Remington and the 1893 Union consolidation) they competed rival layouts out of the market; today they defend the default through product design and ecosystem expectations rather than hardware exclusion. Retooling for an alternative layout is available and cheap by modern standards — which is precisely why their defense now runs through defaults rather than machinery.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, incumbent_keyboard_manufacturers, agenda_setter,
    institutional, generational, arbitrage, global).

% Hold the bulk of their professional skill as layout-specific muscle memory; the standard is what makes that skill portable across every employer and machine. Switching layouts means months of degraded speed and temporary income loss, so they defend the incumbent in hiring expectations and workplace norms even where they hold no rule-making power.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, trained_typists, beneficiary,
    moderate, biographical, constrained, global).

% Business colleges, typing schools, and modern keyboarding curricula sell instruction in the incumbent layout as their core product; accreditation benchmarks and employer expectations are denominated in incumbent-layout speed. Their curricula, textbooks, and certification tests are capital sunk in the standard; teaching an alternative would strand that investment and break the promise their certificates make to employers.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, typing_training_institutions, beneficiary,
    organized, generational, constrained, national).

% Buy typing labor denominated in incumbent-layout speed and benefit from the standardized labor pool the standard creates, but bear the throughput ceiling of the layout and the coordination costs of any deviation — a mixed-layout office cannot share machines, cover absences, or benchmark output. Their efficiency interest pulls toward alternatives; their coordination interest pins them to the default.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, efficiency_seeking_employers, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence__incumbent_preservation_reading, efficiency_seeking_employers, beneficiary).

% Individuals who have learned Dvorak, Colemak, or similar layouts and now bear the friction of non-conformity: shared and public machines, employer typing tests, technical support, printed hardware legends, and collaboration with incumbent-layout colleagues. They have paid the retraining cost and continue paying a compatibility levy the majority does not.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, alternative_layout_adopters, payer,
    moderate, biographical, constrained, global).

% Designers of improved layouts, from Dvorak in 1936 to modern ergonomic communities, whose proposals require exactly the infrastructure — training pipelines, hardware defaults, employer acceptance — that the incumbent arrangement controls. They have no seat in the standardization process; their designs are evaluated, if at all, by the beneficiaries of the incumbent.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, alternative_layout_designers, excluded,
    moderate, biographical, trapped, global).

% Codify the keyboard layout as formal national and international standards. Their ratifications treat the incumbent layout as the reference against which any alternative would be a revision; they have never commissioned an independent efficiency trial of alternatives, and their revision processes give the incumbent the default position in every discussion.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, standards_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Document the layout's history — the typebar-jam origin story, the 1893 consolidation, the Dvorak controversy and its contested studies, the path-dependence debate. They are the seat from which the rival explanations of the persistence record are articulated and tested.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence__incumbent_preservation_reading, diffuse).
narrative_ontology:fixing_cost_class(qwerty_persistence__incumbent_preservation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the skill-portability problem: a single key-to-character mapping lets a typist's training work on any machine and lets employers hire from a pre-trained pool; hardware makers build and stock one layout instead of many; training institutions certify against one benchmark.
% TRANSFER_FUNCTION: Moves the costs of non-conformity onto users of alternative layouts and employers seeking throughput above the incumbent ceiling: they bear retraining costs, hardware and software friction, and foregone gains, while the arrangement shields the sunk capital of manufacturers, typists, and training institutions from transition costs.
% ABSENT_VOICES: Alternative-layout designers and advocates have no seat in the standardization process their proposals must pass through; standards bodies historically ratified the incumbent layout without commissioning independent efficiency trials; typists were never polled on whether they would prefer a retrained future. The excluded seat sits inside the very training and hardware infrastructure the arrangement controls.
% DISAPPEARANCE_RATIONALE: Every shipped keyboard, keycap legend, firmware default, typing curriculum, hiring test, and several hundred million typists' muscle memory would need to re-coordinate overnight; hardware would be mislabeled for years and workplace text output would collapse during retraining. The arrangement is load-bearing for the entire text-input economy.
% FOUNDING_PROBLEM: Typebar jamming on the 1870s Sholes-Glidden machine required a workable key arrangement, and the fragmented 1870s-1890s typewriter market required a single layout to resolve incompatibilities among rival manufacturers — settled by the 1893 Union consolidation on QWERTY.
% FOUNDING_PROBLEM_CORROBORATION: Technology historians and the engineering literature attest the founding problem is dead: typebar mechanisms were superseded by the IBM Selectric (1961) and then by electronic keyboards, and no party outside the beneficiary set attests that jamming or layout fragmentation remains a live problem. The beneficiaries instead attest a successor problem — skill portability — whose very status as the load-bearing justification is the substance of the kernel contest with the lapsed_alternatives_reading.
narrative_ontology:disappearance_verdict(qwerty_persistence__incumbent_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence__incumbent_preservation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence__incumbent_preservation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence__incumbent_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence__incumbent_preservation_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence__incumbent_preservation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence__incumbent_preservation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence__incumbent_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.48: real but bounded. It sums the contested efficiency ceiling of the incumbent layout, the compatibility levy on alternative-layout users, and the defensive expenditure the preservation apparatus consumes (codification cycles that never evaluate alternatives, default-setting, training-pipeline reproduction). It sits well below pure-extraction range because most of the extraction is deadweight friction rather than captured rent, and it has declined from its mid-century peak as software-era switching costs fell. Suppression 0.55: structural, not coercive — factory defaults, printed hardware legends, hiring screens denominated in incumbent-layout speed, accreditation benchmarks. (Suppression is authored as a raw structural property; the engine, not this story, scales extractiveness by directionality and scope.) Theater 0.35 and rising across the series: early defense was substantive (the 1893 consolidation, sales-force exclusivity contracts); modern maintenance increasingly performs justification — ergonomic marketing, standards reviews structured so alternatives never reach the agenda — without re-examining the layout itself. Accessibility_collapse 0.45: alternatives are not collapsed — Dvorak ships in every major OS and Colemak is free — but default infrastructure and hiring expectations collapse their practical adoption. Resistance 0.50: a sustained advocacy tradition from Dvorak (1936) through modern layout communities, plus periodic corporate experiments; real but never coalition-effective, because the same coordination costs the standard imposes on efficiency-seeking employers block the employer-adopter coalition that could force a transition. Receipt surface: gain_flow is authored 'diffuse' as an affirmative finding, not a default — I checked each named seat and none captures the extraction: manufacturers receive protected installed base (a benefit of persistence, not the friction victims pay), typists and schools receive protected asset values, and the compatibility levy itself is deadweight. Historical capture (Remington-era rents) has decayed into distributed protection. fixing_cost is 'prohibitive': retraining the installed typing population and retooling legends, firmware, and curricula dwarfs the contested annual efficiency gain. All three tracked metric series run on one shared time grid (t = years since the 1874 Remington release; points every 25 years) so no metric's end-state value is silently substituted into earlier periods.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different arrangements from the same layout. From the agenda-setter seats (manufacturers, standards bodies) the arrangement is a rational, self-maintaining standard they administer at low cost; from the beneficiary seats (typists, training institutions) it is the guarantee that their sunk human and curricular capital holds value — exit would convert their asset into a loss; from the payer seats (alternative-layout adopters, efficiency-seeking employers) it is a lock that taxes every deviation. The dual-positioned employer seat is the hinge: its efficiency interest pulls against the standard while its coordination interest pins it there, which is why resistance never coalitionizes despite the victims holding organized and moderate power. The engine computes per-seat classifications from these structural asymmetries; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d: manufacturers (agenda_setter, arbitrage-grade exit — they could retool cheaply yet choose the default) sit nearest the beneficiary end; typists and training institutions (constrained exit — their capital is layout-specific) sit beneficiary-side but above the manufacturers because their exit is costlier. Victim declarations drive high d: alternative-layout adopters and efficiency-seeking employers (constrained exit) sit target-side, adopters highest since they have already paid the relearning cost and still pay the compatibility levy. Standards bodies are declared neither beneficiary nor victim; they collect legitimacy and codification relevance from the arrangement's persistence — a low-to-mid d the canonical fallback approximates without an override. No directionality_overrides are authored: the beneficiary/victim declarations plus exit options already produce the correct ordering, and the one genuinely dual-positioned agent (employers) carries secondary_role beneficiary to mark the pull toward symmetry rather than an override that would misstate its net position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — typebar jamming on the 1870s Sholes machine and the fragmentation war among incompatible manufacturer layouts — is dead: typebars vanished with the electric era and fragmentation was settled by the 1893 consolidation. The arrangement persists on a successor coordination function (skill portability across machines and employers) plus active defense of sunk capital. Authoring this as tangled_rope rather than pure extraction prevents mislabeling the genuine, still-valuable portability coordination as extraction; authoring it with a dead founding problem and requires_active_enforcement prevents mislabeling it as pure rope. The R5 mismatch (status dead, verdict world_rearranges) is expected to flag against the computed path — that is the honest signal: this is a coordination function whose original mandate is gone, kept load-bearing by defense, drifting toward inertia if the defense withdraws (see the defense_vs_inertia_ambiguity omega). Mandatrophy is partially resolved: the original mandate is dead, the successor mandate is live, and which one is carrying the arrangement is exactly what the kernel contest is about.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_epsilon_composition,
    'How much of the measured extraction is attributable to the incumbent-defense mechanism this reading instantiates, as opposed to coordination costs any common layout would impose? This story is one reading of the qwerty_persistence kernel; the sibling lapsed_alternatives_reading authors the same persistence facts with lower epsilon and no victim set.',
    'Counterfactual modeling: simulate adoption dynamics under a no-defense counterfactual (factory defaults randomized or chosen on demonstrated merit at each hardware generation) and compare predicted persistence and welfare against the observed record; convergence toward the sibling''s predictions collapses the defensive premium in epsilon.',
    'A small defensive premium collapses this reading toward the sibling (rope-side reclassification; the victim set dissolves into ordinary switching costs); a large premium confirms the tangled_rope structure and opens victim-compensation and standards-process questions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_epsilon_composition, conceptual, 'Whether the defensive-suppression component of epsilon is real or an artifact of the reading.').

omega_variable(
    dvorak_efficiency_magnitude,
    'How large is the true efficiency and ergonomic advantage of Dvorak-class layouts over QWERTY, given that the foundational studies (the 1944 Navy experiment, Everette''s trials) were Dvorak-adjacent and the strongest critique (Liebowitz and Margolis) finds the evidence consistent with near-parity?',
    'Preregistered, independently funded controlled trials randomizing typists across layouts with blind scoring, plus meta-analysis separating authorship-affiliated from independent studies.',
    'Near-parity shrinks the efficiency-tax component of epsilon toward the coordination floor and weakens the victim claim; a substantial advantage makes the standard a standing levy on the entire typing population and raises epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dvorak_efficiency_magnitude, empirical, 'Size of the real efficiency loss the incumbent layout imposes.').

omega_variable(
    defense_vs_inertia_ambiguity,
    'Is modern persistence still produced by active defense (defaults, training pipelines, hiring expectations, codification), or would the standard persist absent any defense — the pure inertia the sibling reading predicts?',
    'Natural experiments where defense is withdrawn or relaxed: organizations and jurisdictions that stopped enforcing QWERTY expectations (remapping-enabled workplaces, alternative-layout-friendly employers) — measure reversion and adoption rates over a decade.',
    'If persistence survives defense withdrawal, the incumbent-preservation mechanism is dying and the arrangement drifts toward the sibling''s account and toward piton-shaped inertia; if persistence tracks the defaults, requires_active_enforcement is load-bearing and this reading''s structure holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(defense_vs_inertia_ambiguity, empirical, 'Whether the enforcement apparatus is still doing causal work.').

omega_variable(
    switching_cost_decomposition,
    'How much of the switching cost borne by would-be adopters is irreducible individual relearning versus socially constructed friction (shared equipment, hiring screens, hardware legends) imposed by the beneficiary network?',
    'Decompose retraining curves in organizations that adopt alternative layouts: individual learning-time component versus coordination-friction component (machine sharing, output benchmarking, technical support).',
    'If most switching cost is socially constructed, the friction is imposed by the beneficiary coalition rather than by nature — sharpening the tangled_rope/snare boundary question; if irreducible, part of the friction is a natural cost no arrangement could remove.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(switching_cost_decomposition, empirical, 'Structural versus natural share of the switching costs victims bear.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__incumbent_preservation_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(qwer_tr_t0, observed).
narrative_ontology:measurement(qwer_tr_t25, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 25, 0.12).
narrative_ontology:measurement_basis(qwer_tr_t25, observed).
narrative_ontology:measurement(qwer_tr_t50, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 50, 0.18).
narrative_ontology:measurement_basis(qwer_tr_t50, observed).
narrative_ontology:measurement(qwer_tr_t75, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 75, 0.3).
narrative_ontology:measurement_basis(qwer_tr_t75, observed).
narrative_ontology:measurement(qwer_tr_t100, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 100, 0.27).
narrative_ontology:measurement_basis(qwer_tr_t100, observed).
narrative_ontology:measurement(qwer_tr_t125, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 125, 0.32).
narrative_ontology:measurement_basis(qwer_tr_t125, observed).
narrative_ontology:measurement(qwer_tr_t150, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 150, 0.35).
narrative_ontology:measurement_basis(qwer_tr_t150, observed).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(qwer_be_t0, observed).
narrative_ontology:measurement(qwer_be_t25, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 25, 0.46).
narrative_ontology:measurement_basis(qwer_be_t25, observed).
narrative_ontology:measurement(qwer_be_t50, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 50, 0.56).
narrative_ontology:measurement_basis(qwer_be_t50, observed).
narrative_ontology:measurement(qwer_be_t75, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 75, 0.62).
narrative_ontology:measurement_basis(qwer_be_t75, observed).
narrative_ontology:measurement(qwer_be_t100, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 100, 0.58).
narrative_ontology:measurement_basis(qwer_be_t100, observed).
narrative_ontology:measurement(qwer_be_t125, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 125, 0.52).
narrative_ontology:measurement_basis(qwer_be_t125, observed).
narrative_ontology:measurement(qwer_be_t150, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 150, 0.48).
narrative_ontology:measurement_basis(qwer_be_t150, observed).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t0, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(qwer_su_t0, observed).
narrative_ontology:measurement(qwer_su_t25, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 25, 0.44).
narrative_ontology:measurement_basis(qwer_su_t25, observed).
narrative_ontology:measurement(qwer_su_t50, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 50, 0.55).
narrative_ontology:measurement_basis(qwer_su_t50, observed).
narrative_ontology:measurement(qwer_su_t75, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 75, 0.66).
narrative_ontology:measurement_basis(qwer_su_t75, observed).
narrative_ontology:measurement(qwer_su_t100, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 100, 0.6).
narrative_ontology:measurement_basis(qwer_su_t100, observed).
narrative_ontology:measurement(qwer_su_t125, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 125, 0.52).
narrative_ontology:measurement_basis(qwer_su_t125, observed).
narrative_ontology:measurement(qwer_su_t150, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 150, 0.55).
narrative_ontology:measurement_basis(qwer_su_t150, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence__incumbent_preservation_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence__incumbent_preservation_reading, qwerty_persistence__lapsed_alternatives_reading).

% DUAL FORMULATION NOTE:
% Family decomposition of the colloquial 'QWERTY persistence' label per the epsilon-invariance principle: the label conflates two structurally distinct claims about one historical record. This story (incumbent_preservation_reading) authors the defense-sustained claim — persistence is produced by incumbent defense; epsilon includes defensive suppression expenditure and imposed switching friction; a victim set exists. The sibling (qwerty_persistence__lapsed_alternatives_reading) authors the coordination-value claim — alternatives lapse for want of critical mass; epsilon sits near the coordination floor; no victim set is declared. The upstream/downstream relation runs both directions in the literature: the sibling's Liebowitz-Margolis critique is cited as evidence against this reading's suppression claims, while David/Arthur-style path-dependence exhibits (this arrangement included) are cited as evidence for them. Each reading's epsilon is stable within its own constraint; the difference is which constraint the label denotes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
