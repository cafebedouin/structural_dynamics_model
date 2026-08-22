% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__became_thinkable_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__became_thinkable_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: electronic_money_emergence__became_thinkable_reading
 *   human_readable: Electronic Money as Conceptual Possibility (Became-Thinkable Reading)
 *   domain: economic_history/monetary_theory
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   'electronic_money_emergence'—specifically, the became-thinkable_reading.
 *   This reading asserts that digital money emerged when the conceptual
 *   possibility became technically and socially thinkable, prior to
 *   institutional measurement. The kernel contest has three readings: (1)
 *   became_thinkable_reading (this file)—emergence marked by
 *   conceptual/technical possibility; (2) first_held_reading
 *   (sibling)—emergence marked by first institutional bearer of
 *   dematerialized currency; (3) m4_m5_collapse_reading (sibling)—emergence
 *   retroactively constructed by M4/M5 statistical distinction. Each reading
 *   is a separate constraint story with its own ε, beneficiary/victim
 *   structure, and classification. They are linked by
 *   network.affects_constraints and differ fundamentally in when emergence
 *   'counts'.
 *
 * KEY AGENTS:
 *   - monetary_economists: benefit from the reading's elevation of conceptual/intellectual work as temporally primary; their research agendas center on possibility-space analysis rather than institutional event-dating
 *   - systems_theorists: benefit from framing emergence as gradual diffusion process; their theories of complex-system evolution are validated by the reading's temporal sequencing
 *   - technical_innovators: benefit from recognition that technical possibility precedes institutional adoption; their innovation cycles are positioned as causally primary; also bear costs when the reading obscures regulatory/coordination barriers they must navigate
 *   - central_banks: pay institutional authority cost; their measurement categories (M0–M5) are redefined as lagging reality rather than constituting it; their monopoly on monetary definition is challenged
 *   - payment_clearing_houses: positioned as technical coordinators enabling institutional phase; do not control the conceptual frame that marks emergence
 *   - financial_regulators: excluded from the definition; their authority to determine what counts as money is severed from the emergence marking
 *   - cryptocurrency_communities: benefit from the reading's emphasis on technical possibility independent of institutional adoption; bear costs when the reading implies their innovations will be institutionalized
 *   - ordinary_account_holders: observer seat; their lived experience and adoption decisions are backgrounded in the conceptual-possibility frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__became_thinkable_reading, 0.38).
domain_priors:suppression_score(electronic_money_emergence__became_thinkable_reading, 0.22).
domain_priors:theater_ratio(electronic_money_emergence__became_thinkable_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__became_thinkable_reading, rope).
narrative_ontology:human_readable(electronic_money_emergence__became_thinkable_reading, "Electronic Money as Conceptual Possibility (Became-Thinkable Reading)").
narrative_ontology:topic_domain(electronic_money_emergence__became_thinkable_reading, "economic_history/monetary_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__became_thinkable_reading, '92e88064-c9bb-42f3-aa57-cec359484303').
narrative_ontology:cs_kernel_codification('92e88064-c9bb-42f3-aa57-cec359484303', distributed).
narrative_ontology:cs_authority_grounding('92e88064-c9bb-42f3-aa57-cec359484303', distributed).
narrative_ontology:cs_reading_relation('92e88064-c9bb-42f3-aa57-cec359484303', electronic_money_emergence__first_held_reading, influences).
narrative_ontology:cs_reading_relation('92e88064-c9bb-42f3-aa57-cec359484303', electronic_money_emergence__m4_m5_collapse_reading, coexists_with).
narrative_ontology:cs_axiom('92e88064-c9bb-42f3-aa57-cec359484303', foundational, emergence_is_conceptual_technical_diffusion).
narrative_ontology:cs_axiom_status(emergence_is_conceptual_technical_diffusion, holdable).
narrative_ontology:cs_axiom_grounding('92e88064-c9bb-42f3-aa57-cec359484303', emergence_is_conceptual_technical_diffusion, empirically_contingent).
narrative_ontology:cs_axiom('92e88064-c9bb-42f3-aa57-cec359484303', secondary, measurement_lags_actual_phenomenon).
narrative_ontology:cs_axiom_status(measurement_lags_actual_phenomenon, holdable).
narrative_ontology:cs_axiom_grounding('92e88064-c9bb-42f3-aa57-cec359484303', measurement_lags_actual_phenomenon, empirically_contingent).
narrative_ontology:cs_reference_frame('92e88064-c9bb-42f3-aa57-cec359484303', possibility_as_emergence_marker).
narrative_ontology:cs_drift_state('92e88064-c9bb-42f3-aa57-cec359484303', contemporary_institutional_dominance, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('92e88064-c9bb-42f3-aa57-cec359484303', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__became_thinkable_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__became_thinkable_reading, monetary_economists).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__became_thinkable_reading, systems_theorists).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__became_thinkable_reading, technical_innovators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__became_thinkable_reading, cryptocurrency_communities).
narrative_ontology:constraint_victim(electronic_money_emergence__became_thinkable_reading, technical_innovators).
narrative_ontology:constraint_victim(electronic_money_emergence__became_thinkable_reading, central_banks).
narrative_ontology:constraint_victim(electronic_money_emergence__became_thinkable_reading, cryptocurrency_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build theoretical frameworks around the possibility space of dematerialized currency and emergence processes. Benefit from the became-thinkable reading's framing because it validates the study of monetary emergence as a conceptual/technical phenomenon prior to institutional deployment, positioning intellectual work as causally primary. Can shift between readings or study multiple frames simultaneously without career penalty.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, monetary_economists, beneficiary,
    organized, generational, mobile, global).

% Use the became-thinkable reading to explain emergence as a diffusion process where ideas precede institutional adoption. Benefit from the reading's emphasis on gradual social-technical coevolution rather than discrete events or measurement artifacts. Academic mobility and theoretical pluralism mean they can adopt or reject the frame without institutional penalty.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, systems_theorists, beneficiary,
    organized, generational, mobile, global).

% Develop cryptographic and network protocols that make digital money technically realizable. Benefit from the reading's recognition that technical possibility precedes institutional adoption—their innovation cycle is validated as temporally primary. Also bear indirect costs: the reading creates expectations that technical innovation alone drives adoption, which obscures regulatory and coordination barriers they then navigate in practice.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, technical_innovators, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(electronic_money_emergence__became_thinkable_reading, technical_innovators, payer).

% Operate under institutional definitions of money supply (M0–M5) that the became-thinkable reading repositions as derivative from and lagging behind actual technical emergence. Bear the institutional cost of their definitional authority being challenged: the reading asserts their measurement categories do not mark emergence, implying they lack monopoly power over what counts as money. Cannot exit the institutional role.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, central_banks, payer,
    institutional, generational, constrained, national).

% Manage settlement and clearing infrastructure for electronic payments. The became-thinkable reading positions them as technical coordinators enabling the institutional phase of a process that already became conceptually possible decades earlier. They set technical standards and infrastructure timing but do not control the conceptual frame that marks emergence.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, payment_clearing_houses, agenda_setter,
    institutional, generational, constrained, national).

% Would argue that emergence should be marked by regulatory recognition and licensed institutional adoption, not by conceptual possibility. Their exclusion from the definition of emergence means their authority to determine what counts as money becomes questionable; the reading severs their definitional gate from the actual phenomenon, undermining their institutional role.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, financial_regulators, excluded,
    institutional, generational, constrained, national).

% Benefit from the reading's emphasis that technical-conceptual possibility precedes and can exist independently of institutional adoption. This supports their claim that digital money is 'real' without central bank backing. Bear costs when the reading's diffusion-process frame implies their innovations will eventually be absorbed into institutional structures, constraining their autonomy narrative and reducing their distinctive position.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, cryptocurrency_communities, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(electronic_money_emergence__became_thinkable_reading, cryptocurrency_communities, payer).

% Use digital banking services without deliberation about the conceptual history of electronic money. The reading affects their experience indirectly: if emergence is positioned as a technical-conceptual phenomenon, their agency in adoption is backgrounded; institutional and regulatory change determine their access, not participatory thinkability.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, ordinary_account_holders, observer,
    powerless, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(electronic_money_emergence__became_thinkable_reading, monetary_economists).
narrative_ontology:fixing_cost_class(electronic_money_emergence__became_thinkable_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared conceptual frame for understanding when digital money 'existed'—solving the coordination problem among theorists, technologists, historians, and policymakers about what event or process marks emergence. Enables multi-disciplinary conversation by centering the intellectual/technical possibility space rather than institutional declaration or statistical measurement, allowing researchers from different traditions to locate emergence at different points in a shared temporal sequence.
% TRANSFER_FUNCTION: Transfers interpretive authority from institutional gatekeepers (central banks, regulators) to technical-conceptual communities (economists, systems theorists, protocol designers). The reading shifts when emergence 'counts' from the moment an institution first held dematerialized currency or from the moment M4/M5 statistical distinction was made, to the moment the possibility became thinkable—a move that advantages the innovators and theorists whose work precedes institutional adoption and de-emphasizes the regulatory recognition that follows.
% ABSENT_VOICES: Financial regulators and central bank officials are structurally excluded from this reading's framing—they would insist that emergence is marked only by institutional recognition and licensed deployment. Ordinary account holders' lived experience of digital money's adoption and use is backgrounded; the reading centers possibility and concept over adoption barriers, user behavior, or practical accessibility. Payment clearing houses are present but marginalized—they appear as technical coordinators rather than as essential agents whose infrastructure decisions shape what becomes thinkable.
% DISAPPEARANCE_RATIONALE: If the became-thinkable reading vanished and only institutional readings remained, the entire historical narrative of electronic money would be rewritten: emergence would date to the first licensed dematerialized account (late 1970s institutional banking) or to M4/M5 statistical distinction (1980s central bank measurement), erasing decades of prior technical and conceptual work. Theorists' research agendas would shift from studying gradual diffusion processes to studying discrete institutional events; university curricula on monetary emergence would reorient toward regulatory history; the professional standing of academics studying possibility-space would diminish relative to institutional historians. The recognition of technical possibility as temporally primary would collapse.
% FOUNDING_PROBLEM: How should we mark the moment digital money 'emerged'? What is the relationship between technical possibility, institutional adoption, and statistical measurement? The became-thinkable reading solves this by repositioning emergence as a gradual diffusion process where conceptual and technical possibility precedes institutional adoption, and institutional measurement lags both.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians (Mehrling, Carruthers, Rogoff) outside the benefiting parties attest that technical and conceptual innovation historically precedes institutional adoption in monetary systems—the reading is corroborated by cross-disciplinary scholarship. Technology historians (Zuboff, Streeter, Abbate) document that telecommunications and information networks were conceptually developed and technically deployed prior to institutional absorption. Central bank economists (Basel, Federal Reserve, Bank of England economic historians) acknowledge in technical papers that their measurement categories lagged observable technical and market change by 10–20 years—external corroboration supports the reading's claim that measurement is not emergence. However, no corroboration exists from the regulatory or central banking authorities themselves—they maintain that institutional recognition is the operational marker, not historical antecedents. The reading is corroborated from academic and historical sources but contested by operational authorities.
narrative_ontology:disappearance_verdict(electronic_money_emergence__became_thinkable_reading, world_rearranges).
narrative_ontology:founding_problem_status(electronic_money_emergence__became_thinkable_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__became_thinkable_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(electronic_money_emergence__became_thinkable_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__became_thinkable_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__became_thinkable_reading_tests).
:- end_tests(electronic_money_emergence__became_thinkable_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end) because the became-thinkable reading benefits a concentrated group (academics, theorists, innovators) by transferring interpretive authority away from institutional gatekeepers, but the extraction is only moderate—the beneficiaries are not capturing resources directly; they are gaining authority to define emergence. Suppression is low (0.22) because the reading faces real resistance: central bankers and regulators actively contest the temporal marking and maintain alternative institutional frameworks. Theater ratio is low (0.18) because the reading is genuinely engaged with historical and technical substance; it is not primarily performative. Accessibility_collapse is moderate (0.45): once the became-thinkable frame is understood, alternatives (institutional dating, measurement-driven dating) remain conceptually accessible but require rejecting the reading's temporal logic. Resistance is moderate-high (0.42) because multiple institutional and disciplinary constituencies have vested interests in alternative temporal markings. The measurements show gradual rise in extractiveness and theater from t=0 to t=30, then stabilization—this reflects increasing academic institutionalization of the became-thinkable frame (more courses, more citations) followed by a plateau as the regulatory-institutional challenge to the frame solidifies.
 *
 * PERSPECTIVAL GAP:
 *   From the academic-theoretical seat (monetary_economists, systems_theorists), the became-thinkable reading is a genuine intellectual discovery—emergence IS marked by conceptual possibility because that is how innovation actually sequences. From the central bank seat, the same reading is a category error—emergence is an institutional phenomenon, and conceptual chatter among academics is not the phenomenon being marked. The engine computes this divergence from power (organized vs. institutional), exit options (mobile vs. constrained), and time horizon (generational for both, but with opposite institutional commitments). The most acute divergence is between technical_innovators and regulators: the innovators see the became-thinkable reading as validating their causal role; regulators see it as erasing their essential gating function.
 *
 * DIRECTIONALITY LOGIC:
 *   Monetary_economists and systems_theorists have directionality near 0.0 (beneficiary)—they gain interpretive authority and professional standing from the reading without bearing direct costs. Their exit options are mobile (they can switch readings or study multiple frames) and their power is organized (disciplinary standing). Technical_innovators sit near 0.3 (partial beneficiary with mild extraction): they benefit from the reading's affirmation of technical-possibility-as-primary but bear costs when the frame creates expectations that technical innovation alone drives adoption, obscuring regulatory barriers. Central_banks and financial_regulators sit near 0.8–0.9 (targets): they bear the institutional cost of challenge to their definitional authority. Their exit options are constrained (they cannot abandon the role of institutional authority) and their time horizons are generational (institutional change is slow). Payment_clearing_houses sit near 0.5 (symmetric): they benefit slightly from coordination emphasis but are constrained by institutional role. Cryptocurrency_communities sit near 0.6 (partial target): they benefit from the independent-adoption narrative but face institutional absorption pressure the reading implies.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (how to mark emergence?) remains live and contested—no single reading has achieved consensus. The founding_problem_status is correctly marked as 'contested' because the three readings actively dispute the temporal marking. The disappearance_verdict is correctly marked as 'world_rearranges' because if the became-thinkable reading vanished, the entire temporal narrative of digital money would shift: academic research agendas would reorient from studying diffusion to studying institutional events; theoretical frameworks would be rewritten; the recognition of technical/conceptual work as causally primary would collapse. This is not mandatrophy (a dead founding problem whose constraint persists). This is a live-contested kernel where three readings of the same founding problem coexist, each with its own constraint structure. The became-thinkable reading does not resolve the contest—it is one pole in an ongoing dispute. Mandatrophy could emerge if the became-thinkable reading eventually loses the contest (if one of the other readings becomes dominant and the became-thinkable reading persists theatrically).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_vs_first_held_boundary,
    'Is ''becoming thinkable'' (conceptual-technical possibility) empirically distinguishable from ''first held institutionally'' (dematerialized account debut)? Do the readings mark a meaningful temporal and causal boundary, or does the distinction dissolve under scrutiny?',
    'Detailed historical reconstruction of when each element became observable: cryptographic publications, network architecture design, first experimental systems, first institutional deployments. Identify the earliest date each community explicitly articulated digital money as possible vs. the earliest date an institution accepted dematerialized funds. Compare the temporal gap to the causal chain—did conceptual clarity enable institutional adoption, or did institutional need drive conceptual framing?',
    'If ''thinkable'' and ''first held'' are empirically separable and temporally ordered (concept first), the became-thinkable reading''s core claim is supported and the two readings coexist as genuinely distinct accounts. If they collapse into the same moment or the causal arrow reverses, the reading''s distinguishing claim evaporates and the readings merge—the became-thinkable reading becomes redundant to the first_held_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_first_held_boundary, empirical, 'Temporal and causal separation of conceptual thinkability from institutional first-holding.').

omega_variable(
    m4_m5_measurement_retroactivity,
    'To what extent does the became-thinkable reading depend on the assumption that measurement categories (M4/M5) are DISCOVERED rather than IMPOSED? If M4/M5 itself is a constructed categorization that retroactively defined what counts as electronic money, does the became-thinkable reading escape this retroactivity, or does it simply push the retroactive moment earlier—from measurement definition to conceptual thinkability definition?',
    'Trace how M4/M5 categories were constructed and debated among central banks and how they were justified (did they reflect pre-existing technical distinctions or did they create them?). Separately trace how ''digital money as concept'' entered economic discourse—was it a stable concept prior to M4/M5, or did M4/M5 retroactively give that concept coherence? Examine whether the became-thinkable reading''s use of ''conceptual possibility'' is itself a measurement construct (one discipline''s way of measuring emergence).',
    'If both the became-thinkable reading and the m4_m5_collapse_reading are true (measurement retroactively constructs emergence), then the became-thinkable reading does not escape the retroactivity problem—it simply stages it at the level of conceptual-community consensus rather than institutional-statistical consensus. The readings would influence each other rather than coexist as independent accounts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(m4_m5_measurement_retroactivity, conceptual, 'Whether the became-thinkable reading itself embeds a retroactive measurement construct (''conceptual possibility'') that mirrors the retroactivity it attributes to M4/M5.').

omega_variable(
    beneficiary_capture_and_theoretical_framing,
    'The became-thinkable reading benefits monetary economists, systems theorists, and technical innovators whose professional standing depends on the intellectual/technical preceding institutional categories. To what extent does this beneficiary structure contaminate the reading—does the reading''s appeal to theorists and technologists rest on the reading''s accuracy, or partly on its institutional positioning (which privileges the communities who profit from the possibility-centered frame)?',
    'Examine whether central bank economists and regulators (the payers in this reading) produced alternative temporal markings of emergence and whether their alternatives are systematically neglected in the academy. Test whether the became-thinkable reading persists in domains (e.g., practical policy, financial stability analysis) where theorists have less professional standing. Compare citation patterns and institutional adoption: is the reading cited more by academics than by practitioners?',
    'High evidence that the reading''s adoption tracks beneficiary interest rather than empirical accuracy would elevate the concern that this constraint is partly extractive—the reading transfers interpretive authority to academics at the expense of regulators'' operational perspective. This would raise the extracted_extraction parameter and suggest the measured rope classification masks asymmetric institutional positioning.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_and_theoretical_framing, empirical, 'Whether beneficiary capture affects the became-thinkable reading''s empirical claim or institutional persistence.').

omega_variable(
    social_thinkability_vs_technical_possibility,
    'The reading claims digital money became ''technically and socially thinkable''—but what makes something socially thinkable? Does social thinkability require peer-reviewed publication, university-course integration, policy-brief circulation, or merely theoretical articulation by any community member? The boundary between ''social possibility'' and ''individual speculation'' is unspecified.',
    'Define operationally: when did digital money transition from ''thought by some specialists'' to ''socially available as a possibility''? Identify the earliest texts, conferences, and communities where the idea was articulated and shared. Track when the idea entered non-specialist discourse (business press, policy circles, public debate). Use that moment as the empirical marker of social thinkability and compare it to technical innovation markers and institutional adoption.',
    'Absent a clear boundary, ''socially thinkable'' becomes hermeneutically flexible—it can be pushed earlier (to any specialist articulation) or later (to mass-market discourse) depending on argumentative need. This flexibility makes the reading vulnerable to retrospective coherence-adjustment (fitting the thinkability date to match the desired narrative rather than discovering it).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(social_thinkability_vs_technical_possibility, conceptual, 'Operational definition of when digital money became socially (not just technically) thinkable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__became_thinkable_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_tr_t0, electronic_money_emergence__became_thinkable_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(elec_tr_t10, electronic_money_emergence__became_thinkable_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(elec_tr_t20, electronic_money_emergence__became_thinkable_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(elec_tr_t30, electronic_money_emergence__became_thinkable_reading, theater_ratio, 30, 0.16).
narrative_ontology:measurement(elec_tr_t45, electronic_money_emergence__became_thinkable_reading, theater_ratio, 45, 0.17).
narrative_ontology:measurement(elec_tr_t60, electronic_money_emergence__became_thinkable_reading, theater_ratio, 60, 0.18).

% Extraction over time
narrative_ontology:measurement(elec_be_t0, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(elec_be_t10, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(elec_be_t20, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 20, 0.31).
narrative_ontology:measurement(elec_be_t30, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(elec_be_t45, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 45, 0.38).
narrative_ontology:measurement(elec_be_t60, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 60, 0.38).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(electronic_money_emergence__became_thinkable_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__became_thinkable_reading, information_standard).
narrative_ontology:boltzmann_floor_override(electronic_money_emergence__became_thinkable_reading, 0.05).
narrative_ontology:affects_constraint(electronic_money_emergence__became_thinkable_reading, electronic_money_emergence__first_held_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__became_thinkable_reading, electronic_money_emergence__m4_m5_collapse_reading).

% DUAL FORMULATION NOTE:
% The became-thinkable_reading is one of three constraint stories decomposing the contested kernel electronic_money_emergence. Each reading offers a different temporal marking of when digital money 'emerged': (1) conceptual/technical possibility (this story), (2) first institutional holding (first_held_reading), (3) measurement retroactivity (m4_m5_collapse_reading). They are linked because each reading implicitly contests the others' temporal claim. The became-thinkable reading influences the others by establishing a temporal sequence (possibility → holding → measurement) that the other readings either accept (and locate emergence at a different point in the sequence) or reject (and argue for simultaneity or retroactivity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
