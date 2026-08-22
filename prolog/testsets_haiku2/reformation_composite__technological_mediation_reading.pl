% ============================================================================
% CONSTRAINT STORY: reformation_composite__technological_mediation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__technological_mediation_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: reformation_composite__technological_mediation_reading
 *   human_readable: Printing Press as Enabling Constraint for Continental Religious Movement
 *   domain: historical_epistemology/technological_mediation
 *
 * SUMMARY:
 *   This constraint story instantiates the technological-mediation reading of
 *   the Reformation kernel: the claim that the printing press is the primary
 *   determinant of the Reformation's ability to scale from local theological
 *   dissent to a continental mass movement. The reading asserts that before
 *   print, theological innovation remained locally trapped; printing enabled
 *   simultaneous mass engagement with authoritative texts across dispersed
 *   geographies, creating the possibility of a unified continental argument
 *   movement. This reading privileges technical causation (reproduction,
 *   distribution, literacy infrastructure) as the primary explanatory axis
 *   and backgrounds theological content and political realignment as
 *   downstream effects of technological enablement. The constraint is
 *   authored as a mountain: printing press as a natural law-like constraint
 *   that persists regardless of observer, with high accessibility collapse
 *   (alternatives are unavailable before the technology exists) and low
 *   resistance (the technology operates by physical causation, not through
 *   contested rules). The beneficiary declaration (literate_lay_audiences)
 *   triggers FSM evaluation: genuine mountains do not have organized
 *   beneficiaries; the presence of a beneficiary documents an omega-class
 *   ambiguity about whether this is true natural law or a constructed
 *   institutional choice using the naturalness framing.
 *
 * KEY AGENTS:
 *   - printing_press_operators: moderate power, entrepreneurs and institutional printers controlling reproduction technology and access to means of mass distribution
 *   - literate_lay_audiences: organized power, direct beneficiaries of vernacular text access and individual textual engagement
 *   - ecclesiastical_authority: institutional power, loses monopoly on textual mediation but attempts suppression through censorship and prohibition
 *   - theological_dissenters: moderate power, gain unprecedented access to mass reproduction and capacity to create continental argument networks
 *   - manuscript_scribes_copyists: moderate power, see their labor obsolesced by mechanized reproduction
 *   - illiterate_populations: powerless, excluded from the textual constraint but affected downstream by social upheaval
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__technological_mediation_reading, 0.18).
domain_priors:suppression_score(reformation_composite__technological_mediation_reading, 0.12).
domain_priors:theater_ratio(reformation_composite__technological_mediation_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, accessibility_collapse, 0.89).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, resistance, 0.06).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__technological_mediation_reading, mountain).
narrative_ontology:human_readable(reformation_composite__technological_mediation_reading, "Printing Press as Enabling Constraint for Continental Religious Movement").
narrative_ontology:topic_domain(reformation_composite__technological_mediation_reading, "historical_epistemology/technological_mediation").

domain_priors:emerges_naturally(reformation_composite__technological_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__technological_mediation_reading, '0c985b25-622a-4bbc-98d4-025a74515931').
narrative_ontology:cs_kernel_codification('0c985b25-622a-4bbc-98d4-025a74515931', distributed).
narrative_ontology:cs_authority_grounding('0c985b25-622a-4bbc-98d4-025a74515931', distributed).
narrative_ontology:cs_reading_relation('0c985b25-622a-4bbc-98d4-025a74515931', reformation_composite__theological_fragmentation_reading, influences).
narrative_ontology:cs_reading_relation('0c985b25-622a-4bbc-98d4-025a74515931', reformation_composite__political_realignment_reading, influences).
narrative_ontology:cs_axiom('0c985b25-622a-4bbc-98d4-025a74515931', foundational, printing_press_enables_continental_coordination).
narrative_ontology:cs_axiom_status(printing_press_enables_continental_coordination, holdable).
narrative_ontology:cs_axiom_grounding('0c985b25-622a-4bbc-98d4-025a74515931', printing_press_enables_continental_coordination, empirically_contingent).
narrative_ontology:cs_axiom('0c985b25-622a-4bbc-98d4-025a74515931', foundational, mass_reproduction_is_primary_causation).
narrative_ontology:cs_axiom_status(mass_reproduction_is_primary_causation, holdable).
narrative_ontology:cs_axiom_grounding('0c985b25-622a-4bbc-98d4-025a74515931', mass_reproduction_is_primary_causation, empirically_contingent).
narrative_ontology:cs_reference_frame('0c985b25-622a-4bbc-98d4-025a74515931', manuscript_gatekeeping_authority_structure).
narrative_ontology:cs_drift_state('0c985b25-622a-4bbc-98d4-025a74515931', mid_sixteenth_century_print_normalization, gap(codification_collapse, substantial, true)).
narrative_ontology:cs_created_at('0c985b25-622a-4bbc-98d4-025a74515931', '').
narrative_ontology:cs_kernel_id(reformation_composite__technological_mediation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, literate_lay_audiences).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, theological_dissenters).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, ecclesiastical_authority).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, manuscript_scribes_copyists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the technology of reproduction. Decide which texts to print, in which languages, at what cost. Early printers are independent entrepreneurs seeking profit; later institutional printers serve state or church interests. Their decisions shape what reaches which audiences.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, printing_press_operators, agenda_setter,
    moderate, biographical, mobile, regional).

% For the first time in European history, gain access to sacred texts and theological argument in vernacular language at a cost that permits individual possession. Before printing, literacy was gatekept by scribal reproduction and clerical mediation; print enables direct encounter with source material and simultaneous engagement across dispersed communities.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, literate_lay_audiences, beneficiary,
    organized, biographical, mobile, continental).

% Loses monopoly on textual mediation and theological interpretation. The printing press enables rapid dissemination of texts the Church did not authorize, in languages and contexts it does not control. Authority attempted to suppress printing (prohibition, licensing, censorship lists) but could not prevent its adoption or replication. The constraint operates at the scale the Church cannot scale enforcement to match.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, ecclesiastical_authority, payer,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(reformation_composite__technological_mediation_reading, ecclesiastical_authority, agenda_setter).

% See their labor obsolesced by mechanized reproduction. Manuscript copying was a skilled trade; print made the skill valueless for volume reproduction. Scribes retained roles in specialized contexts (manuscripts, illumination, bureaucratic copying) but lost market position to printers.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, manuscript_scribes_copyists, payer,
    moderate, biographical, constrained, local).

% Gain unprecedented access to means of mass reproduction. A single authorship act — writing a tract, reformulation of doctrine — can be copied thousands of times and distributed across Christendom within months, creating a distributed community of readers encountering the same argument simultaneously. Before print, dissent remained local or required scribal networks.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, theological_dissenters, beneficiary,
    moderate, generational, mobile, continental).

% Remain outside the constraint: they cannot read printed texts and so do not directly benefit from or participate in the textual Reformation, despite their involvement in subsequent social upheaval. Religious change reaches them through oral preaching, visual imagery, and social upheaval triggered by the literate movement.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, illiterate_populations, excluded,
    powerless, generational, trapped, continental).

% The physical and social preconditions (papermaking, typography, distribution networks, baseline literacy) that enable the press to function. Not an agent in the constraint but a precondition measured analytically.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, print_literacy_infrastructure, observer,
    analytical, civilizational, analytical, continental).
narrative_ontology:stakeholder_non_agent(reformation_composite__technological_mediation_reading, print_literacy_infrastructure).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_composite__technological_mediation_reading, diffuse).
narrative_ontology:fixing_cost_class(reformation_composite__technological_mediation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables simultaneous mass engagement with the same texts across dispersed geographies — solves the coordination problem of how theological argument can reach and persuade a continental audience instead of remaining local theological debate.
% TRANSFER_FUNCTION: Moves textual authority from scribal-mediated and gatekept channels (ecclesiastical) to mechanically reproduced channels accessible to literate lay audiences. Transfers interpretive labor from ecclesiastical specialists to individual readers. Transfers economic value from manuscript scribes to printers.
% ABSENT_VOICES: Illiterate populations (majority of Europe) have no seat at this constraint: they do not read, do not author printed texts, and do not participate in the textual argument. Their engagement with Reformation change comes later, through oral preaching and social upheaval triggered by the literate movement — a different constraint, not this one.
% DISAPPEARANCE_RATIONALE: If the printing press technology had not been invented, the theological arguments would not disappear — they would remain embedded in manuscript networks, scribal reproduction chains, and oral transmission. The Reformation's shape and scale would be radically different (local, slower, more easily suppressed), but theological dissent is not a creation of print technology; print is the mechanism that amplified dissent into a continental movement.
% FOUNDING_PROBLEM: Before print: sacred texts are scarce, controlled by institutional copyists, and mediated through clerical interpretation. Theological innovation in one locality cannot reach other dissenting communities efficiently. Authority over text and interpretation is gatekept to the trained clergy. The founding problem is not the theological questions themselves, but the structural isolation of dissent and the institutional monopoly on textual reproduction.
% FOUNDING_PROBLEM_CORROBORATION: Literacy historians and print historians (Eisenstein, Pettegree, Swanson) document the pre-print scarcity of texts and the clerical monopoly on sacred manuscript production. The founding problem is attested by the observable fact that theological dissent existed before print (pre-Gutenberg heresy movements, local reform initiatives) but could not scale to continental organization until print enabled mass reproduction. The problem remains live: in contemporary contexts without press access, theological innovation remains locally trapped.
narrative_ontology:disappearance_verdict(reformation_composite__technological_mediation_reading, world_unchanged).
narrative_ontology:founding_problem_status(reformation_composite__technological_mediation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__technological_mediation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reformation_composite__technological_mediation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__technological_mediation_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__technological_mediation_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, ExtMetricName, E),
    domain_priors:suppression_score(reformation_composite__technological_mediation_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(reformation_composite__technological_mediation_reading),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(reformation_composite__technological_mediation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the printing press itself does not extract in the conventional sense — it is a neutral enabling technology. What is measured as extractiveness is the asymmetry that emerges: ecclesiastical authority loses monopoly on mediation (cost born), literate lay audiences gain access (benefit collected), printers gain economic opportunity. Suppression is low (0.12) because the constraint operates through physical causation (the technology works) rather than through coercive rules. Suppression rises from 0.05 to 0.13 mid-interval (1500s) because ecclesiastical authorities invest in active suppression (censorship lists, licensing, prohibition) in response to uncontrolled printing — but suppression requirement never reaches high levels because the technology's distribution is decentralized (multiple printing centers) and difficult to control at continental scale. Theater ratio is low (0.08) because the printing press does genuine functional work (text reproduction) rather than performative work. Accessibility collapse is very high (0.89) because once printing technology exists, the alternative of manuscript copying at scale is not credible — the technology's efficiency obliges adoption. Resistance is very low (0.06) because the physical causation of printing meets almost no organized opposition; what does oppose (ecclesiastical suppression) operates as enforcement overhead, not as meaningful resistance to the technology itself. The measurement series show extractiveness and suppression rising through the 1500s as ecclesiastical response intensifies, then stabilizing as the technology becomes normalized.
 *
 * PERSPECTIVAL GAP:
 *   From the ecclesiastical seat, the printing press appears as a loss of authority and a threat requiring active suppression — high directionality toward extraction, high enforcement cost. From the literate lay audience seat, it appears as genuine enablement and access — low or negative directionality (beneficiary end). From the printer seat, it is neither extraction nor coordination but economic opportunity. The engine should compute these seats differently from the same structural data: high directionality for the ecclesiastical authority (target of the technology's displacement effects), low directionality for lay audiences (beneficiaries), neutral for printers (neither deeply extracting nor deeply benefiting — moderate power, mobile exit).
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical authority is the structural target: loses monopoly, bears enforcement costs, cannot prevent the displacement. High directionality toward extraction (d ~ 0.75). Literate lay audiences are the structural beneficiaries: gain access, no exit costs, direct gain from coordination. Low directionality (d ~ 0.15). Theological dissenters are secondary beneficiaries: gain means of reproduction but also depend on literate audiences' adoption. Moderate beneficiary directionality (d ~ 0.30). Manuscript scribes are collateral targets: labor obsolesced without choice. High extraction directionality (d ~ 0.80). Printers are agenda-setters but not deep extractors: they make profit but from the technology's general efficiency, not from deliberately suppressing alternatives. Moderate directionality (d ~ 0.45). The beneficiary declaration (literate_lay_audiences) in base_properties is structural: these agents genuinely benefit from the constraint's operation and would defend its persistence. This triggers FSM evaluation because a mountain-classified constraint should not have organized beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The Reformation's founding problem (scarcity of texts, gatekept textual authority, isolation of dissent) remains live in contemporary contexts without press access or without literacy. The constraint does not show mandatrophy: the founding problem persists, and the printing press remains the primary mechanism for solving it at scale. No omega marks the constraint as dead-but-persisting.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_determinism_vs_enabling_condition,
    'Is the printing press a determining cause of the Reformation''s continental scale, or an enabling condition whose impact depends on pre-existing theological and social vectors?',
    'Comparative historical analysis: regions with printing access but low literacy and weak dissent networks did not experience Reformation movements at the same scale or speed; regions with high literacy and active theological networks but delayed printing access saw slower movement formation. The covariance of printing adoption with movement emergence is measurable but does not settle causation direction.',
    'If determining (printing makes the Reformation inevitable given theological input), the constraint is a mountain-grade natural law. If merely enabling (printing amplifies existing vectors but does not generate them), the constraint is a rope-grade coordination mechanism whose primary function is amplification. The reading asserts mountain status; this omega documents the reduction to rope as a live interpretive possibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_determinism_vs_enabling_condition, conceptual, 'Whether print is a determinant or enabler of continental movement formation.').

omega_variable(
    literacy_as_boundary_condition,
    'Is the printing press constraint genuinely a mountain that persists regardless of literacy, or does literacy become a co-determining factor that shares dominion over the constraint''s effects?',
    'Historical counterfactuals: abundant printed texts in a sub-literate population (16th-century regions with low literacy rates) did not produce the same continental argument networks as regions with adequate literacy. At what literacy threshold does print transition from neutral technical capability to active movement amplifier? The constraint may be conditionally natural: mountain at high literacy, rope or inert at low literacy.',
    'If literacy is a co-determinant, the constraint is not a pure mountain but a mountain-conditioned-on-literacy, which weakens the naturalness claim and potentially reclassifies to rope. If print alone determines the effect regardless of literacy, the mountain claim holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_as_boundary_condition, empirical, 'Whether literacy thresholds materially affect print''s constraint strength.').

omega_variable(
    reading_vs_theological_authenticity,
    'Is the technological-mediation reading an accurate characterization of what made the Reformation continental, or is it a reductionist framing that misses the theological and political vectors that were themselves necessary?',
    'Engage the sibling readings (theological_fragmentation_reading, political_realignment_reading) as falsifiers: if those readings produce materially different predictions about which dissents succeeded and which failed, at what scale, and in which geographies, then technological mediation alone is insufficient and the reading is one axis among co-equal determinants, not the primary frame.',
    'If the sibling readings are equally necessary to explain the Reformation''s shape, this reading is one facet of a multi-cause kernel decomposition, not a competing account. If technological mediation alone predicts the empirical pattern of movement emergence and spread better than sibling readings alone, the reading''s claim is validated. If no single reading predicts well but combinations do, the kernel is genuinely multifactorial.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_vs_theological_authenticity, conceptual, 'Whether technological mediation is the primary determinant or one co-equal axis among multiple.').

omega_variable(
    printing_press_naturalness_claim,
    'Is the printing press a natural law (physics of mechanical reproduction, inevitable discovery given paper and metalworking) or a constructed human choice to invest in the technology and deploy it for religious texts?',
    'Historical anthropology of technology adoption: the printing press was invented and then was NOT universally adopted (Islamic scholars and Ottoman scribes initially rejected it; Chinese printing technology predates Gutenberg; reasons for adoption/rejection vary by institutional choice). The press is a natural technical capability but not a natural inevitability — it becomes constraining only through deployment choices. This is the FSM candidate: beneficiary (literate_lay_audiences) appears in an otherwise mountain-like structure.',
    'If printing press is a constructed institutional choice, the constraint is a false summit: a beneficiary structure riding on a claim of naturalness. Reclassification would shift from mountain to tangled_rope or snare, depending on whether print technology genuinely coordinates (rope) or extracts (snare). The FSM signature should fire here.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(printing_press_naturalness_claim, conceptual, 'Whether the printing press is a natural law or a constructed institutional choice.').

omega_variable(
    kernel_decomposition_framing_dependence,
    'Is the technological-mediation reading a genuine structural frame or a framing choice that privileges technical causation over theological and political causation for rhetorical reasons?',
    'Examine the alternative kernels (theological_fragmentation_reading, political_realignment_reading): do they describe genuinely different structural dynamics that would produce different empirical predictions, or are they mere redescriptions of the same underlying events? If the three readings are genuinely orthogonal (different causal chains, different outcome-sensitive variables, different prediction sets), the kernel decomposition is real. If they are rhetorical reframings of a single phenomenon, the kernel is under-specified and the decomposition is an artifact of framing choice, not structure.',
    'If the readings are orthogonal, the technological-mediation reading is a valid one-axis decomposition. If they are rhetorical reframings, the reading is a perspectival choice, not a structural claim, and the mountain classification becomes observer-dependent rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_decomposition_framing_dependence, preference, 'Whether kernel decomposition reflects genuine structural orthogonality or rhetorical reframing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__technological_mediation_reading, 1440, 1550).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1440, reformation_composite__technological_mediation_reading, theater_ratio, 1440, 0.05).
narrative_ontology:measurement_basis(refo_tr_t1440, projected).
narrative_ontology:measurement(refo_tr_t1460, reformation_composite__technological_mediation_reading, theater_ratio, 1460, 0.06).
narrative_ontology:measurement_basis(refo_tr_t1460, observed).
narrative_ontology:measurement(refo_tr_t1480, reformation_composite__technological_mediation_reading, theater_ratio, 1480, 0.07).
narrative_ontology:measurement_basis(refo_tr_t1480, observed).
narrative_ontology:measurement(refo_tr_t1500, reformation_composite__technological_mediation_reading, theater_ratio, 1500, 0.08).
narrative_ontology:measurement_basis(refo_tr_t1500, observed).
narrative_ontology:measurement(refo_tr_t1520, reformation_composite__technological_mediation_reading, theater_ratio, 1520, 0.09).
narrative_ontology:measurement_basis(refo_tr_t1520, observed).
narrative_ontology:measurement(refo_tr_t1550, reformation_composite__technological_mediation_reading, theater_ratio, 1550, 0.08).
narrative_ontology:measurement_basis(refo_tr_t1550, observed).

% Extraction over time
narrative_ontology:measurement(refo_be_t1440, reformation_composite__technological_mediation_reading, base_extractiveness, 1440, 0.08).
narrative_ontology:measurement_basis(refo_be_t1440, projected).
narrative_ontology:measurement(refo_be_t1460, reformation_composite__technological_mediation_reading, base_extractiveness, 1460, 0.12).
narrative_ontology:measurement_basis(refo_be_t1460, observed).
narrative_ontology:measurement(refo_be_t1480, reformation_composite__technological_mediation_reading, base_extractiveness, 1480, 0.15).
narrative_ontology:measurement_basis(refo_be_t1480, observed).
narrative_ontology:measurement(refo_be_t1500, reformation_composite__technological_mediation_reading, base_extractiveness, 1500, 0.18).
narrative_ontology:measurement_basis(refo_be_t1500, observed).
narrative_ontology:measurement(refo_be_t1520, reformation_composite__technological_mediation_reading, base_extractiveness, 1520, 0.19).
narrative_ontology:measurement_basis(refo_be_t1520, observed).
narrative_ontology:measurement(refo_be_t1550, reformation_composite__technological_mediation_reading, base_extractiveness, 1550, 0.18).
narrative_ontology:measurement_basis(refo_be_t1550, observed).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1440, reformation_composite__technological_mediation_reading, suppression_requirement, 1440, 0.05).
narrative_ontology:measurement_basis(refo_su_t1440, projected).
narrative_ontology:measurement(refo_su_t1460, reformation_composite__technological_mediation_reading, suppression_requirement, 1460, 0.09).
narrative_ontology:measurement_basis(refo_su_t1460, observed).
narrative_ontology:measurement(refo_su_t1480, reformation_composite__technological_mediation_reading, suppression_requirement, 1480, 0.11).
narrative_ontology:measurement_basis(refo_su_t1480, observed).
narrative_ontology:measurement(refo_su_t1500, reformation_composite__technological_mediation_reading, suppression_requirement, 1500, 0.12).
narrative_ontology:measurement_basis(refo_su_t1500, observed).
narrative_ontology:measurement(refo_su_t1520, reformation_composite__technological_mediation_reading, suppression_requirement, 1520, 0.13).
narrative_ontology:measurement_basis(refo_su_t1520, observed).
narrative_ontology:measurement(refo_su_t1550, reformation_composite__technological_mediation_reading, suppression_requirement, 1550, 0.12).
narrative_ontology:measurement_basis(refo_su_t1550, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__technological_mediation_reading, information_standard).
narrative_ontology:boltzmann_floor_override(reformation_composite__technological_mediation_reading, 0.05).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, reformation_composite__theological_fragmentation_reading).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, reformation_composite__political_realignment_reading).

% DUAL FORMULATION NOTE:
% The Reformation kernel decomposes into three orthogonal readings: technological_mediation_reading (this file) privileges technical causation and reproduction mechanics; theological_fragmentation_reading privileges doctrinal incompatibility and soteriological vectors; political_realignment_reading privileges state sovereignty and institutional realignment. Each reading instantiates a different constraint with different ε, different beneficiary/victim structures, and different primary mechanisms. All three are live interpretive positions in Reformation scholarship. This reading influences both siblings by providing the precondition (mass text distribution) that allows both theological and political vectors to operate at continental scale; neither sibling could produce their effects without the technological substrate this reading describes. Theological and political readings do not foreclose this reading — they reframe its role as enabling rather than determining.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
