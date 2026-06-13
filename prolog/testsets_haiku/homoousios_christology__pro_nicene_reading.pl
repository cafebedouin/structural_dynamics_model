% ============================================================================
% CONSTRAINT STORY: homoousios_christology__pro_nicene_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_christology__pro_nicene_reading, []).

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
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
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
 *   constraint_id: homoousios_christology__pro_nicene_reading
 *   human_readable: Pro-Nicene Homoousios Christology (identical divine substance)
 *   domain: historical/theological/ecclesiastical
 *
 * SUMMARY:
 *   The Council of Nicaea (325 CE) declares that Christ is homoousios
 *   (consubstantial—of identical divine substance) with the Father. This
 *   pro-Nicene reading instantiates one interpretation of a contested
 *   theological kernel: the metaphysical relationship between Father and Son.
 *   The competing readings are Arianism (Christ is created and subordinate,
 *   homoios rather than homoousios) and semi-Arianism (Christ is of similar
 *   but not identical substance, homoiousios). The pro-Nicene reading is
 *   enforced through imperial decree, episcopal hierarchy, exile of
 *   dissenters, confiscation of Arian church property, and ecclesiastical
 *   anathema. The constraint operates as tangled_rope: a genuine coordination
 *   problem (unified doctrine for imperial stability) coupled with asymmetric
 *   extraction (imperial-church alignment benefits some bishops and the
 *   imperial administration at the cost of dissenting theologians and
 *   identity-locked Arian communities). The claim/metric divergence is
 *   intentional: the pro-Nicene bishops present homoousios as a theological
 *   truth discovered and agreed upon; the authored metrics reflect that it
 *   persists through active enforcement, rising suppression as resistance
 *   persists, and increasing theater (performative doctrinal recitations by
 *   coerced conformists).
 *
 * KEY AGENTS:
 *   - imperial_ecclesiastical_authority: Constantine and successor emperors; set the agenda through convening councils and enforcing decrees
 *   - nicene_council_bishops: Beneficiary-agenda-setters; gain ecclesiastical power, imperial patronage, and authority to anathematize
 *   - arian_theologians_and_communities: Identity-locked payers; face exile, property loss, and theological suppression despite coherent scriptural arguments
 *   - dissenting_regional_bishops: Constrained payers; navigating choice between conformity-via-coercion and removal-via-resistance
 *   - non_greek_speaking_churches: Dual-positioned; receive coordination benefit (unified doctrine) but experience extraction (foreign theological imposition)
 *   - alternative_theological_schools: Excluded; Antioch and Alexandria schools subordinated to imperial authority despite sophisticated theology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_christology__pro_nicene_reading, 0.68).
domain_priors:suppression_score(homoousios_christology__pro_nicene_reading, 0.79).
domain_priors:theater_ratio(homoousios_christology__pro_nicene_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__pro_nicene_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_christology__pro_nicene_reading, "Pro-Nicene Homoousios Christology (identical divine substance)").
narrative_ontology:topic_domain(homoousios_christology__pro_nicene_reading, "historical/theological/ecclesiastical").

domain_priors:requires_active_enforcement(homoousios_christology__pro_nicene_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__pro_nicene_reading, '2e1bbe45-cd13-4e90-9b84-0dd14c60280f').
narrative_ontology:cs_kernel_codification('2e1bbe45-cd13-4e90-9b84-0dd14c60280f', formalized).
narrative_ontology:cs_authority_grounding('2e1bbe45-cd13-4e90-9b84-0dd14c60280f', extraction).
narrative_ontology:cs_interpretation_layer_present('2e1bbe45-cd13-4e90-9b84-0dd14c60280f').
narrative_ontology:cs_reading_relation('2e1bbe45-cd13-4e90-9b84-0dd14c60280f', arian_christology__arian_reading, forecloses).
narrative_ontology:cs_reading_relation('2e1bbe45-cd13-4e90-9b84-0dd14c60280f', homoiousios_christology__semi_arian_reading, coexists_with).
narrative_ontology:cs_axiom('2e1bbe45-cd13-4e90-9b84-0dd14c60280f', foundational, christ_divine_equality).
narrative_ontology:cs_axiom_status(christ_divine_equality, holdable).
narrative_ontology:cs_axiom_grounding('2e1bbe45-cd13-4e90-9b84-0dd14c60280f', christ_divine_equality, deontological).
narrative_ontology:cs_axiom('2e1bbe45-cd13-4e90-9b84-0dd14c60280f', foundational, one_substance_doctrine).
narrative_ontology:cs_axiom_status(one_substance_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('2e1bbe45-cd13-4e90-9b84-0dd14c60280f', one_substance_doctrine, conventional).
narrative_ontology:cs_reference_frame('2e1bbe45-cd13-4e90-9b84-0dd14c60280f', apostolic_tradition_christ_divinity).
narrative_ontology:cs_drift_state('2e1bbe45-cd13-4e90-9b84-0dd14c60280f', post_nicene_enforcement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2e1bbe45-cd13-4e90-9b84-0dd14c60280f', '').
narrative_ontology:cs_kernel_id(homoousios_christology__pro_nicene_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, imperial_ecclesiastical_authority).
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, nicene_council_bishops).
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, imperial_administration).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, arian_theologians_and_communities).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, dissenting_regional_bishops).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, non_greek_speaking_churches).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, non_greek_speaking_churches).
narrative_ontology:constraint_vindicates(homoousios_christology__pro_nicene_reading, christ_divine_equality).
narrative_ontology:constraint_vindicates(homoousios_christology__pro_nicene_reading, trinitarian_unity_doctrine).
narrative_ontology:constraint_vindicates(homoousios_christology__pro_nicene_reading, imperial_religious_uniformity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Constantine and successor emperors summon and oversee ecumenical councils, enforce doctrinal decisions through imperial decree, exile dissenting bishops, and control church property and resources. They frame doctrinal uniformity as essential to imperial stability and religious order. The homoousios formulation serves both theological precision and centralizing political authority: one substance doctrine = one church = one empire.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, imperial_ecclesiastical_authority, agenda_setter,
    institutional, generational, arbitrage, continental).

% The bishops who frame and vote for the homoousios formulation at Nicaea (325 CE) and subsequent councils gain ecclesiastical authority, imperial patronage, and the power to anathematize competitors. They claim to defend apostolic tradition and Christ's divinity against degradation. Their authority depends on the emperor's enforcement machinery; without imperial backing, their pronouncements have only theological weight.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, nicene_council_bishops, beneficiary,
    powerful, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__pro_nicene_reading, nicene_council_bishops, agenda_setter).

% Arian and subordinationist theologians and their congregations are declared heretical, their churches confiscated, their clergy exiled, and their theological writings burned. They maintain that their interpretation is faithful to scripture and reason; the homoousios doctrine is imposed through coercion, not theological victory. Many hold their position as identity-constitutive: their theology defines their faith and their community, making exit equivalent to apostasy.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, arian_theologians_and_communities, payer,
    powerful, biographical, identity_locked, continental).

% Bishops who prefer subordinationism, semi-arianism, or resist imperial interference in doctrine face exile, deposition, and loss of see. They navigate a choice between conformity (signing the homoousios formula), resistance (being removed), or flight to unconquered regions. Their regional authority is contingent on imperial recognition; without it, they lose both legitimacy and material resources.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, dissenting_regional_bishops, payer,
    powerful, biographical, constrained, regional).

% Churches in Syria, Egypt, Persia, and other non-Greek regions receive the homoousios doctrine as an imperial and Greek-episcopal imposition. The formulation is foreign to their theological traditions and linguistic categories; they comply to avoid imperial sanction but maintain reservations. Over time, many drift toward semi-arian or subordinationist theology, viewed by the imperial church as heresy.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, non_greek_speaking_churches, payer,
    moderate, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__pro_nicene_reading, non_greek_speaking_churches, beneficiary).

% The imperial bureaucracy uses ecclesiastical uniformity as a tool of territorial control: a single authorized doctrine, enforced through a centralized church hierarchy, reduces dissent and resistance to imperial policy. Theology becomes a lever of statecraft. The homoousios doctrine, tied to imperial authority, becomes an instrument of provincial governance.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, imperial_administration, beneficiary,
    institutional, generational, analytical, continental).

% Schools like Antioch (emphasizing Christ's distinction from the Father) and Alexandria (emphasizing union) produce sophisticated theology on the nature of Christ but are subordinated to the imperial-council authority. Their scholarship is devalued if it does not conform to homoousios orthodoxy. Dissenting theologians face exclusion from imperial patronage and teaching positions.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, alternative_theological_schools, excluded,
    moderate, biographical, trapped, regional).

% Bishops and theologians attending the councils hold the constraint under scrutiny: they author it, debate it, vote on it, and experience its consequences. Their dissent or assent becomes a permanent record; voting against homoousios carries risk of exile or deposition. Their observational seat reveals the coercive machinery within what is presented as conciliar consensus.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, ecumenical_council_participants, observer,
    analytical, biographical, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_christology__pro_nicene_reading, imperial_ecclesiastical_authority).
narrative_ontology:fixing_cost_class(homoousios_christology__pro_nicene_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles the theological question of Christ's relationship to the Father in terms acceptably precise to Greek philosophical discourse and imperial religious authority: one substance (ousia) doctrine rules out Arianism (which makes Christ created and subordinate) and provides a shared vocabulary across regional churches for discussing Christological identity.
% TRANSFER_FUNCTION: Transfers ecclesiastical authority from regional theological schools and independent bishops to an imperial-backed, ecumenical conciliar hierarchy. Transfers confiscated Arian church property to Nicene clergy. Transfers exegetical authority from competing theological traditions to the imperial-approved reading. Transfers dissenting bishops and theologians from sees and influence to exile or deposition.
% ABSENT_VOICES: Arian and subordinationist congregations are excluded from the council's decision-making despite their theological sophistication and scriptural arguments. Non-Greek-speaking churches that experience homoousios as an imposed foreign formulation are not primary architects. Lay believers who understand the doctrine neither philologically nor theologically are not consulted. Alternative philosophical schools (Neoplatonism, Middle Platonism) that inform the theological debate are cited but subordinated.
% DISAPPEARANCE_RATIONALE: If homoousios doctrine and its enforcement vanished, the Arian and subordinationist churches would recover legitimacy and property; regional theological autonomy would reassert; the ecumenical council system would lose its centralizing authority; the imperial church-state alignment would weaken. Eastern and Western Christianity would reorganize around competing Christological formulations without a single enforced standard.
% FOUNDING_PROBLEM: The question of Christ's metaphysical relationship to the Father had generated competing interpretations (Arianism, Sabellianism, Adoptionism, Subordinationism) that fractured church unity and created theological confusion. Imperial political stability required a unified, standardized doctrine that could be enforced across the empire and taught in a single authorized form.
% FOUNDING_PROBLEM_CORROBORATION: The imperial authorities and Nicene bishops attest the founding problem is live and ongoing—dissent requires perpetual enforcement. Arian and subordinationist theologians attest the founding problem is artificially constructed: their theology offers a coherent reading of scripture and tradition, and unification through imposed doctrine is a solution to an invented problem. Later church historians (Eusebius, Jerome, Athanasius) attest both interpretations from their respective positions; modern historical scholarship outside the ecclesiastical framework notes that the founding problem was genuinely disputed and that Nicene enforcement was political as well as theological.
narrative_ontology:disappearance_verdict(homoousios_christology__pro_nicene_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_christology__pro_nicene_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_christology__pro_nicene_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(homoousios_christology__pro_nicene_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_christology__pro_nicene_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_christology__pro_nicene_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_christology__pro_nicene_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 (pre-council theological debate) to 0.68 (fully enforced imperial doctrine) across the interval: initially, Arianism and Nicene theology compete on textual and philosophical grounds with no clear victor; by interval end, the homoousios formulation is imposed by imperial force, backed by exile, property confiscation, and burned heretical texts. Suppression climbs from 0.55 to 0.79, tracking enforcement infrastructure maturation: the imperial machinery learns to identify and punish dissent more effectively. Theater rises from 0.15 to 0.42: early debate is genuine theological engagement; by later stages, much episcopal activity is performative recitation of orthodoxy under coercion. The coercion grid captures level-resolved dynamics: at the structural level (imperial decree, church hierarchy), accessibility collapse and suppression reach near-maximum (0.81, 0.79) by interval end, while resistance declines (0.48); at the organizational level (bishop seats), the same pattern holds but less extreme; at the class level (regional theological communities), alternatives remain more accessible (0.79 collapse, but higher resistance 0.31); at the individual level, the constraint is least accessible to personal dissent (0.76 collapse by end) but resistance remains highest (0.38) because internalization is incomplete—identity-locked Arians maintain theological commitment despite suppression. All measurements on one shared time grid (seven time points: 0, 8, 16, 24, 32, 40, 50) so every metric is authored at every point.
 *
 * PERSPECTIVAL GAP:
 *   The imperial-ecclesiastical agenda-setters and beneficiary bishops compute the constraint as rope (coordination + benefit). From their seat, homoousios solves a real theological problem (multiple competing Christologies fracture unity), accomplishes genuine coordination (unified doctrine enables unified church hierarchy), and benefits them individually and institutionally. The arian_theologians and constrained dissenting bishops compute it as snare (extraction + suppression). From their seats, the homoousios formulation is theologically inferior (subordinationist readings are coherent and scripturally grounded), imposed by coercion rather than theological argument, and creates only extraction (loss of legitimacy, property, see). The non_greek_speaking churches sit in between: they receive coordination benefit (a single authorized doctrine simplifies administration and reduces internal conflict) but experience extraction (a foreign theological imposition with limited local legitimacy). The engine derives this seat-level divergence from the authored beneficiary/victim declarations and power atoms; the claim (tangled_rope) sits between the rope intuition of beneficiaries and the snare intuition of victims.
 *
 * DIRECTIONALITY LOGIC:
 *   The imperial administration and Nicene council bishops are the structural beneficiaries (d near 0.0-0.2): they set the agenda, enforce the doctrine, accrue institutional power, and are never threatened with exile or deposition for their position. Arian theologians are the structural targets (d near 0.8-1.0): they are exiled, their property is confiscated, their theological authority is destroyed, and their exit options are severely constrained by identity-lock (renouncing Arianism is renouncing their theological identity and community). Dissenting regional bishops are partially targeted (d near 0.6-0.75): they face pressure to conform but retain some exit options (they can flee to unconquered regions, negotiate with rival emperors, or gradually fade from authority). Non-Greek churches are near-symmetric (d near 0.45-0.55): they gain coordination benefit but bear modest extraction costs (conformity required, local theology devalued). The directionality_overrides remain empty: the structural derivation from beneficiary/victim + exit options produces accurate d values without override.
 *
 * MANDATROPHY ANALYSIS:
 *   The pro-Nicene reading avoids false summit classification by declaring both beneficiaries and victims in base_properties, ensuring the constraint is not misread as a natural law or pure coordination. Mandatrophy (doctrine outliving its function) appears as a risk: the founding problem is empire-level religious unity, live throughout the interval in the historical record. However, the founding_problem_status is authored as contested precisely because by the later interval the theological debates have been settled by enforcement rather than argument—Arian resistance persists even when imperial enforcement weakens, suggesting the founding problem (doctrinal unity) has not actually been solved by the homoousios formulation, only enforced. A piton trajectory would emerge if enforcement machinery decayed without the constraint becoming a genuine rope through theologians changing minds; instead, the historical record shows Arian theology resurging whenever imperial enforcement weakens, supporting the tangled_rope classification (enforcement-dependent, not consensus-dependent).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_vs_political_motivation,
    'Is the homoousios formulation a genuine theological solution to an authentic metaphysical problem, or is it primarily a political instrument of imperial control dressed in theological language?',
    'Comparative analysis of the sophistication and internal coherence of Arian, semi-Arian, and Nicene theological arguments independent of enforcement context. Examination of whether Nicene bishops prioritize theological precision or imperial alignment when the two conflict.',
    'If genuinely theological, the classification remains tangled_rope (coordination with extraction). If primarily political, it tilts toward snare (extraction with theological cover). The reading''s own legitimacy depends on this distinction being resolvable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theological_vs_political_motivation, conceptual, 'Whether homoousios solves a real theological problem or is primarily a political control mechanism.').

omega_variable(
    arian_suppression_internalization,
    'Among identity-locked Arian theologians and communities, is the measured suppression (0.79) structural (enforced exile, property loss, church confiscation) or internalized (the Arian tradition has fused with their identity such that renouncing it is unthinkable)?',
    'Examine what happens when enforcement declines (e.g., during periods of weak imperial authority or friendly Arian emperors): do suppressed communities quickly re-emerge (structural) or do they remain self-suppressed (internalized)? Historical record shows robust Arian recovery when enforcement weakens, suggesting suppression is largely structural, not internalized.',
    'If largely structural, the constraint persists only through active enforcement; if largely internalized, the constraint carries forward even after enforcement machinery deteriorates. This affects piton vs. tangled_rope trajectory under enforcement decay.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(arian_suppression_internalization, empirical, 'Mechanism of suppression: structural coercion or internalized identity-fusion.').

omega_variable(
    nicene_consensus_vs_coerced_conformity,
    'Did the homoousios formulation achieve genuine theological consensus among participating bishops, or was the Nicene Council result coerced through imperial pressure and threat of exile?',
    'Examination of bishops'' subsequent theological writings, their private letters, their behavior under different imperial regimes. Historical record (Eusebius of Caesarea''s account, Jerome''s later reflections) documents that many bishops signed against their theological preference under imperial duress. The question is whether this duress represents forced conversion or coerced conformity to an already-marginal position.',
    'If genuine consensus, homoousios is a rope (coordinated). If coerced, it is tangled_rope (coordinated framing + extracted conformity) or snare (extraction alone). The reading''s classification depends on whether the council represents horizontal agreement or vertical enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nicene_consensus_vs_coerced_conformity, empirical, 'Whether the Nicene Council produced genuine theological consensus or enforced conformity.').

omega_variable(
    kernel_contest_ontology,
    'This constraint is one reading of the homoousios kernel; two sibling readings instantiate the same kernel differently. The question: are the three readings genuinely different instantiations of one contested claim, or are they three entirely separate claims that happen to use overlapping language?',
    'Examine whether changing from pro-Nicene to Arian to semi-Arian reading would preserve the core theological framework or require abandoning foundational commitments. If the three readings can be held within one interpretive tradition (as subsequent theology attempts), the kernel is genuinely contested. If they represent incommensurable frameworks, they are three separate constraints.',
    'The committer frame assumes one kernel with three readings. If the readings are actually incommensurable, this constraint story should decompose into three separate constraint stories with no kernel_id linking them. The network structure depends on this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_ontology, conceptual, 'Whether the three readings are genuine variants of one contested kernel or three separate constraint claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__pro_nicene_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t0, homoousios_christology__pro_nicene_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(homo_tr_t0, projected).
narrative_ontology:measurement(homo_tr_t8, homoousios_christology__pro_nicene_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement_basis(homo_tr_t8, observed).
narrative_ontology:measurement(homo_tr_t16, homoousios_christology__pro_nicene_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement_basis(homo_tr_t16, observed).
narrative_ontology:measurement(homo_tr_t24, homoousios_christology__pro_nicene_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement_basis(homo_tr_t24, observed).
narrative_ontology:measurement(homo_tr_t32, homoousios_christology__pro_nicene_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement_basis(homo_tr_t32, observed).
narrative_ontology:measurement(homo_tr_t40, homoousios_christology__pro_nicene_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(homo_tr_t40, observed).
narrative_ontology:measurement(homo_tr_t50, homoousios_christology__pro_nicene_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(homo_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(homo_be_t0, homoousios_christology__pro_nicene_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(homo_be_t0, projected).
narrative_ontology:measurement(homo_be_t8, homoousios_christology__pro_nicene_reading, base_extractiveness, 8, 0.51).
narrative_ontology:measurement_basis(homo_be_t8, observed).
narrative_ontology:measurement(homo_be_t16, homoousios_christology__pro_nicene_reading, base_extractiveness, 16, 0.59).
narrative_ontology:measurement_basis(homo_be_t16, observed).
narrative_ontology:measurement(homo_be_t24, homoousios_christology__pro_nicene_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement_basis(homo_be_t24, observed).
narrative_ontology:measurement(homo_be_t32, homoousios_christology__pro_nicene_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement_basis(homo_be_t32, observed).
narrative_ontology:measurement(homo_be_t40, homoousios_christology__pro_nicene_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(homo_be_t40, observed).
narrative_ontology:measurement(homo_be_t50, homoousios_christology__pro_nicene_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(homo_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t0, homoousios_christology__pro_nicene_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(homo_su_t0, projected).
narrative_ontology:measurement(homo_su_t8, homoousios_christology__pro_nicene_reading, suppression_requirement, 8, 0.66).
narrative_ontology:measurement_basis(homo_su_t8, observed).
narrative_ontology:measurement(homo_su_t16, homoousios_christology__pro_nicene_reading, suppression_requirement, 16, 0.74).
narrative_ontology:measurement_basis(homo_su_t16, observed).
narrative_ontology:measurement(homo_su_t24, homoousios_christology__pro_nicene_reading, suppression_requirement, 24, 0.78).
narrative_ontology:measurement_basis(homo_su_t24, observed).
narrative_ontology:measurement(homo_su_t32, homoousios_christology__pro_nicene_reading, suppression_requirement, 32, 0.79).
narrative_ontology:measurement_basis(homo_su_t32, observed).
narrative_ontology:measurement(homo_su_t40, homoousios_christology__pro_nicene_reading, suppression_requirement, 40, 0.79).
narrative_ontology:measurement_basis(homo_su_t40, observed).
narrative_ontology:measurement(homo_su_t50, homoousios_christology__pro_nicene_reading, suppression_requirement, 50, 0.79).
narrative_ontology:measurement_basis(homo_su_t50, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=50
narrative_ontology:measurement(homo_grid_01, homoousios_christology__pro_nicene_reading, accessibility_collapse(class), 0, 0.55).
narrative_ontology:measurement(homo_grid_02, homoousios_christology__pro_nicene_reading, accessibility_collapse(class), 50, 0.79).
narrative_ontology:measurement(homo_grid_03, homoousios_christology__pro_nicene_reading, accessibility_collapse(individual), 0, 0.42).
narrative_ontology:measurement(homo_grid_04, homoousios_christology__pro_nicene_reading, accessibility_collapse(individual), 50, 0.76).
narrative_ontology:measurement(homo_grid_05, homoousios_christology__pro_nicene_reading, accessibility_collapse(organizational), 0, 0.72).
narrative_ontology:measurement(homo_grid_06, homoousios_christology__pro_nicene_reading, accessibility_collapse(organizational), 50, 0.87).
narrative_ontology:measurement(homo_grid_07, homoousios_christology__pro_nicene_reading, accessibility_collapse(structural), 0, 0.68).
narrative_ontology:measurement(homo_grid_08, homoousios_christology__pro_nicene_reading, accessibility_collapse(structural), 50, 0.81).
narrative_ontology:measurement(homo_grid_09, homoousios_christology__pro_nicene_reading, resistance(class), 0, 0.58).
narrative_ontology:measurement(homo_grid_10, homoousios_christology__pro_nicene_reading, resistance(class), 50, 0.31).
narrative_ontology:measurement(homo_grid_11, homoousios_christology__pro_nicene_reading, resistance(individual), 0, 0.64).
narrative_ontology:measurement(homo_grid_12, homoousios_christology__pro_nicene_reading, resistance(individual), 50, 0.38).
narrative_ontology:measurement(homo_grid_13, homoousios_christology__pro_nicene_reading, resistance(organizational), 0, 0.68).
narrative_ontology:measurement(homo_grid_14, homoousios_christology__pro_nicene_reading, resistance(organizational), 50, 0.41).
narrative_ontology:measurement(homo_grid_15, homoousios_christology__pro_nicene_reading, resistance(structural), 0, 0.72).
narrative_ontology:measurement(homo_grid_16, homoousios_christology__pro_nicene_reading, resistance(structural), 50, 0.48).
narrative_ontology:measurement(homo_grid_17, homoousios_christology__pro_nicene_reading, stakes_inflation(class), 0, 0.48).
narrative_ontology:measurement(homo_grid_18, homoousios_christology__pro_nicene_reading, stakes_inflation(class), 50, 0.63).
narrative_ontology:measurement(homo_grid_19, homoousios_christology__pro_nicene_reading, stakes_inflation(individual), 0, 0.35).
narrative_ontology:measurement(homo_grid_20, homoousios_christology__pro_nicene_reading, stakes_inflation(individual), 50, 0.54).
narrative_ontology:measurement(homo_grid_21, homoousios_christology__pro_nicene_reading, stakes_inflation(organizational), 0, 0.64).
narrative_ontology:measurement(homo_grid_22, homoousios_christology__pro_nicene_reading, stakes_inflation(organizational), 50, 0.81).
narrative_ontology:measurement(homo_grid_23, homoousios_christology__pro_nicene_reading, stakes_inflation(structural), 0, 0.58).
narrative_ontology:measurement(homo_grid_24, homoousios_christology__pro_nicene_reading, stakes_inflation(structural), 50, 0.72).
narrative_ontology:measurement(homo_grid_25, homoousios_christology__pro_nicene_reading, suppression(class), 0, 0.49).
narrative_ontology:measurement(homo_grid_26, homoousios_christology__pro_nicene_reading, suppression(class), 50, 0.77).
narrative_ontology:measurement(homo_grid_27, homoousios_christology__pro_nicene_reading, suppression(individual), 0, 0.38).
narrative_ontology:measurement(homo_grid_28, homoousios_christology__pro_nicene_reading, suppression(individual), 50, 0.71).
narrative_ontology:measurement(homo_grid_29, homoousios_christology__pro_nicene_reading, suppression(organizational), 0, 0.58).
narrative_ontology:measurement(homo_grid_30, homoousios_christology__pro_nicene_reading, suppression(organizational), 50, 0.82).
narrative_ontology:measurement(homo_grid_31, homoousios_christology__pro_nicene_reading, suppression(structural), 0, 0.62).
narrative_ontology:measurement(homo_grid_32, homoousios_christology__pro_nicene_reading, suppression(structural), 50, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__pro_nicene_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(homoousios_christology__pro_nicene_reading, 0.12).
narrative_ontology:affects_constraint(homoousios_christology__pro_nicene_reading, arian_christology__arian_reading).
narrative_ontology:affects_constraint(homoousios_christology__pro_nicene_reading, homoiousios_christology__semi_arian_reading).
narrative_ontology:affects_constraint(homoousios_christology__pro_nicene_reading, imperial_religious_uniformity_policy).
narrative_ontology:affects_constraint(homoousios_christology__pro_nicene_reading, ecumenical_council_authority_structure).

% DUAL FORMULATION NOTE:
% The homoousios_christology kernel decomposes into three structurally distinct constraint stories, one per reading: pro_nicene (this story, high enforcement ε), arian (competing reading, politically defeated but theologically coherent), and semi_arian (compromise position). Each reading instantiates the same kernel with different beneficiary/victim structures and different ε values. The three readings are linked via network.affects_constraints to model the kernel contest: pro-Nicene enforcement suppresses Arian and semi-Arian alternatives. Each story is independent and ε-invariant; the constraint family structure captures the theological and political interdependence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
