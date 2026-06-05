% ============================================================================
% CONSTRAINT STORY: viability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_viability_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: viability_reading
 *   human_readable: Viability-Based Personhood Boundary and State Restriction Authority
 *   domain: constitutional_law/bioethics/moral_philosophy
 *
 * SUMMARY:
 *   The viability reading instantiates one reading of the contested kernel
 *   'personhood_boundary'—the claim that moral status begins when a fetus
 *   acquires the capacity for independent survival, approximately at 24 weeks
 *   of gestation when fetal lungs and central nervous system have developed
 *   sufficiently for survival outside the uterus with medical support. This
 *   reading grounds legitimate state authority to restrict abortion
 *   post-viability while preserving pre-viability abortion access under
 *   maternal autonomy doctrine. The constraint exhibits tangled rope
 *   structure: genuine coordination need (recognizing viable fetus interests
 *   in continued development) is embedded in asymmetric extraction (state
 *   enforcement authority without balanced countervailing maternal voice in
 *   post-viability restrictions). Suppression is high because the viability
 *   threshold creates a discontinuous boundary: pre-viability the fetus has
 *   no legal status; post-viability the state gains restrictive authority.
 *   This discontinuity enables different suppression regimes operating on
 *   either side of the boundary, with ambiguity about which barriers serve
 *   coordination and which serve extraction. The theater ratio (0.48)
 *   reflects moderate performative content in medical authority's invocation
 *   of viability as a 'biological fact' rather than a normative
 *   threshold—clinicians reference viability as if it is purely scientific,
 *   disguising the normative boundary-setting work. The constraint's
 *   extractiveness has increased over the measurement interval (0.35 to 0.58)
 *   as state restrictions have proliferated: the viability threshold has been
 *   used to justify not only post-viability restrictions but also
 *   pre-viability barriers (waiting periods, counseling mandates, parental
 *   notification) that operate outside the explicit moral-status language,
 *   effectively expanding suppression without explicit victim acknowledgment.
 *
 * KEY AGENTS:
 *   - Pre-viability pregnant agents: Primary victims (powerless/trapped) — bear suppression (legal barriers, cost barriers, informational mandates) while fetus has no legal personhood status; cannot exit jurisdiction without high cost
 *   - Viable fetus (interests-based): Enters victim set at viability; has recognized interests in continued development but no voice in state restrictions that may override maternal autonomy
 *   - State regulatory authority: Primary beneficiary (institutional/arbitrage) — gains legitimate authority to restrict abortion post-viability and enforce compliance through law; controls the boundary definition
 *   - Liberal autonomy coalition: Powerful organized agents (powerful/mobile) — see viability as coordination point (preserving pre-viability access while acknowledging post-viability interests) but experience extraction through state barriers that exceed coordination requirements
 *   - Fetal personhood coalition: Organized agents (organized/constrained) — constrained by legal boundary that delays moral status recognition; see extraction in pre-viability permissions
 *   - Medical authority system: Institutional actor (institutional/arbitrage) — uses viability as functional proxy in clinical practice; authority has largely atrophied to courts and legislatures (piton perspective); maintains performative neutrality
 *   - Analytical observer: Civilizational context (analytical/analytical) — risks naturalizing the contingent 24-week threshold as a biological fact rather than recognizing it as a normative choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(viability_reading, 0.58).
domain_priors:suppression_score(viability_reading, 0.65).
domain_priors:theater_ratio(viability_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(viability_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(viability_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(viability_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(viability_reading, tangled_rope).
narrative_ontology:human_readable(viability_reading, "Viability-Based Personhood Boundary and State Restriction Authority").
narrative_ontology:topic_domain(viability_reading, "constitutional_law/bioethics/moral_philosophy").

domain_priors:requires_active_enforcement(viability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(viability_reading, formalized).
narrative_ontology:cs_authority_grounding(viability_reading, lineage).
narrative_ontology:cs_kernel_id(viability_reading, personhood_boundary).
narrative_ontology:cs_reading_relation(viability_reading, conception_reading, coexists_with).
narrative_ontology:cs_reading_relation(viability_reading, birth_reading, coexists_with).
narrative_ontology:cs_axiom(viability_reading, foundational, independent_survival_capacity_moral_marker).
narrative_ontology:cs_axiom_status(independent_survival_capacity_moral_marker, holdable).
narrative_ontology:cs_axiom(viability_reading, foundational, maternal_autonomy_pre_viability).
narrative_ontology:cs_axiom_status(maternal_autonomy_pre_viability, holdable).
narrative_ontology:cs_reference_frame(viability_reading, viability_based_personhood_recognition).
narrative_ontology:cs_drift_state(viability_reading, contemporary_nicu_expansion_era, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(viability_reading, state_regulatory_authority).
narrative_ontology:constraint_beneficiary(viability_reading, maternal_autonomy_framework).
narrative_ontology:constraint_victim(viability_reading, viable_fetus_moral_status).
narrative_ontology:constraint_victim(viability_reading, pre_viability_abortion_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRE-VIABILITY PREGNANT AGENT (SNARE) — Trapped by viability threshold: personhood status does not attach until 24 weeks, leaving pre-viability fetus without victim status, yet state may still impose significant extraction (waiting periods, counseling mandates, financial barriers, parental notification) that constrains reproductive autonomy without corresponding moral status protection for the fetus. The pregnant agent bears suppression (legal barriers, cost barriers, informational mandates) with no countervailing victim-protection rhetoric. Maximum extraction from this perspective.
constraint_indexing:constraint_classification(viability_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: VIABLE FETUS (INTERESTS-BASED TANGLED ROPE) — Viability threshold creates hybrid structure: at viability, fetus enters the victim set with recognized interests; state gains legitimate grounds to protect those interests through birth restrictions or delivery mandates. But the coordination function (recognizing viable fetus interests in continued development) is asymmetrically extracted: state exercises enforcement (birth restrictions, medical intervention authority) with minimal countervailing pregnant agent voice. Genuine coordination need (viable fetus survival interest) embedded in asymmetric extraction (state authority to compel or restrict).
constraint_indexing:constraint_classification(viability_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGULATORY STATE AUTHORITY (ROPE) — Viability threshold provides clear coordination mechanism: state interest in fetal life (post-viability) aligns with fetal interest in continued development. The state experiences this as pure coordination — a rational boundary for exercising protective authority. State has maximal arbitrage (enforcement power, legitimacy to restrict), experiences low effective extraction because the coordination is framed as unidirectional obligation (fetal protection) rather than symmetric cost-sharing. State sees the constraint as functional and legitimate.
constraint_indexing:constraint_classification(viability_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LIBERAL AUTONOMY COALITION (TANGLED ROPE) — Powerful organized actors (reproductive rights advocates, liberal jurisprudence) see viability as a coordination point: it preserves pre-viability abortion access while acknowledging post-viability fetal interests. But the extraction is real: state still imposes barriers (waiting periods, counseling mandates, parental notification, funding restrictions) that operate across both the pre-viability and post-viability regimes. The coordination function (balancing fetal interests post-viability with maternal autonomy pre-viability) is asymmetrically extracted: state enforcement authority exceeds what coordination logic requires. Mobile exit (can cross state lines, can challenge in courts) keeps chi moderate.
constraint_indexing:constraint_classification(viability_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: FETAL PERSONHOOD COALITION (TANGLED ROPE) — Organized actors (pro-life groups, religious institutions) see viability as an inadequate personhood threshold; their reading would place moral status at conception (different kernel reading). From this perspective, the viability reading coordinates genuine moral interests (fetal protection) but extracts from the fetus by delaying status recognition until viability. Suppression is real (legal barriers to pre-viability protection mechanisms); coordination exists (post-viability protection is recognized). Exit is constrained: changing the legal boundary requires major legislative or constitutional action. These actors see extraction they cannot easily escape.
constraint_indexing:constraint_classification(viability_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: MEDICAL AUTHORITY SYSTEM (PITON) — Medical establishment uses viability as a functional proxy for personhood in clinical practice (NICU resuscitation thresholds, withdrawal-of-care protocols, informed consent procedures), but the institution has largely ceded legitimate authority over the boundary to courts and legislatures. Medical theater persists: clinicians reference 'viability' as if it is a biological fact rather than a normative threshold, maintaining the fiction of neutrality. The medical system's previous function (adjudicating fetal status through clinical judgment) has atrophied, replaced by legal determination. Theater ratio is moderate—some performative invocation of medical authority, but courts have largely displaced it.
constraint_indexing:constraint_classification(viability_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / BIOLOGICAL BOUNDARY VIEW (MOUNTAIN) — From civilizational analytical scope, viability appears as a natural biological threshold: the capacity for independent survival is an objective fact at ~24 weeks (lung development, thermoregulation, neurological integration). This perspective treats viability as a natural law of biology, not a constructed moral boundary. However, the false summit detection will trigger here: identifiable beneficiaries (state regulatory authority, certain abortion-restricting coalitions) exist, and the viability threshold enables their authority. The 'natural boundary' framing naturalizes what is actually a contingent institutional choice.
constraint_indexing:constraint_classification(viability_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(viability_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(viability_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(viability_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(viability_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(viability_reading, TR),
    TR >= 0.70.

:- end_tests(viability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The viability reading enables state authority to restrict abortion post-viability (genuine extraction from the pregnant agent) while preserving a coordination function (recognizing viable fetus interests). The extractiveness value reflects that the coordination is real but asymmetric: the state has enforcement power and fetus has recognized interests, but the pregnant agent's voice in balancing these interests post-viability is structurally diminished. The upward trajectory from 0.35 to 0.58 reflects the historical pattern whereby the viability threshold has been used to justify expanding pre-viability restrictions (waiting periods, counseling mandates, funding restrictions) that operate outside explicit moral-status language, effectively creeping suppression without explicit victim acknowledgment. Suppression (0.65): High. Barriers include legal restrictions post-viability (state can mandate birth, restrict delivery methods, compel medical intervention), legal barriers pre-viability (waiting periods, counseling requirements, parental notification where applicable, funding restrictions), informational mandates (state-scripted counseling content), and jurisdictional barriers (inability to access less restrictive abortion care in other states without high cost). The discontinuity at viability creates two distinct suppression regimes. Theater ratio (0.48): Moderate. The medical authority system performs viability as a 'biological fact' when it is actually a normative threshold. Courts invoke 'viability' as if it is discovered rather than constructed. The performative content has increased over time as courts have elaborated viability restrictions with diminishing connection to actual fetal medical capacity (e.g., restricting abortion for fetal anomalies incompatible with life, based on viability language rather than explicit moral reasoning).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a maximal perspectival gap. The state authority sees rope (pure coordination: recognizing fetal interests provides a clear, functional boundary for state protective authority). The liberal autonomy coalition sees tangled rope (genuine coordination need embedded in asymmetric extraction: state restrictions exceed what the coordination logic requires). The pre-viability pregnant agent sees snare (trapped by suppression with no countervailing victim status for the fetus). The fetal personhood coalition sees tangled rope but experienced as extraction (genuine fetal interests are recognized only after viability, meaning pre-viability 'permissions' for abortion represent extraction from the fetus's perspective). The medical system sees piton (performative invocation of viability while actual authority has been displaced to courts). The analytical observer risks seeing mountain (viability as a natural biological fact) but the false summit detector identifies this as naturalization of a contingent threshold. The perspectival gaps reveal that the viability reading's classification depends entirely on the observer's structural position relative to the state's authority and the competing victim sets (pre-viability pregnant agent vs viable fetus).
 *
 * DIRECTIONALITY LOGIC:
 *   The viability reading's directionality structure is complex because it operates at multiple levels. At the state level: state authority is the beneficiary (d ≈ 0.05, low extraction for state), while pregnant agents are victims (d ≈ 0.85, high extraction). At the fetal level: fetus enters victim set post-viability with recognized interests; state has authority (institutional power); fetus has no exit options (trapped by biology and law), so d ≈ 0.95 for the fetus. At the coalition level: liberal autonomy coalition experiences moderate extraction (mobile exit—can cross state lines, pursue litigation—keeps d moderate, around 0.55); fetal personhood coalition experiences extraction (constrained exit—changing the legal boundary requires major legislative action—keeps d around 0.70). The directionality values derive from beneficiary/victim declarations and exit options without need for overrides; the structural data (state is beneficiary, pregnant agents and fetus are victims; state has arbitrage exit while pre-viability agents are trapped, viable fetus is trapped by biology) determines the chi scaling.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    viability_biological_vs_normative,
    'Is viability (~24 weeks fetal lung and neurological development) a biological fact or a normative threshold that society has chosen?',
    'Examine the contingency of the 24-week marker: viability improves continuously from 20-28 weeks; the 24-week ''cutoff'' is jurisdictional (varies by state and nation) and has shifted historically as NICU technology improved. If the marker had no normative content, it would be invariant across jurisdictions and time periods.',
    'If biological fact: viability reading is a mountain (natural boundary), and other readings (conception, birth) are departures from nature. If normative threshold: viability reading is tangled rope at best, snare at worst, and the false summit detection correctly reclassifies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(viability_biological_vs_normative, conceptual, 'Whether viability is biological fact or normative construction').

omega_variable(
    state_authority_grounding,
    'Does the viability threshold provide legitimate state authority to restrict abortion post-viability, or does it merely relocate extraction authority to the state?',
    'Compare state restrictiveness post-viability across jurisdictions with genuine fetal interests (e.g., does state mandate birth in cases of severe fetal anomaly? does state restrict maternal choice in cases of genuine medical emergency?). If state restrictions exceed what coordination logic requires, the threshold enables extraction rather than legitimate protection.',
    'If legitimate: tangled rope with genuine coordination function is correct; state restrictions post-viability serve both fetal and maternal interests. If extraction authority: snare or worse from fetal perspective; state uses viability as cover to expand restriction authority beyond what fetal interests require.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_authority_grounding, empirical, 'Whether post-viability state restrictions serve fetal interests or enable extraction').

omega_variable(
    pre_viability_suppression_mechanism,
    'Does the viability reading reduce pre-viability suppression, or does it enable a different form of extraction (regulatory barriers that operate outside moral status language)?',
    'Historical comparison: are pre-viability abortion access restrictions lower in viability-based jurisdictions than in conception-based jurisdictions? Or do viability jurisdictions simply replace fetal-protection language with state police power language, achieving equivalent suppression through different mechanisms (waiting periods, counseling mandates, funding restrictions)?',
    'If viability reduces suppression: the reading succeeds in its coordination logic (lower barriers pre-viability). If viability enables functional equivalence in suppression: the reading merely disguises extraction under different normative framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pre_viability_suppression_mechanism, empirical, 'Whether viability reading reduces pre-viability abortion suppression').

omega_variable(
    moral_status_boundary_alternative_framings,
    'What alternative moral-status boundaries exist, and how would they change the victim/beneficiary structure?',
    'Explicit comparison of conception reading (victim set at conception), viability reading (victim set at viability), and birth reading (victim set at birth) as three separate constraints with different ε values and victim structures. The choice between readings is not reducible to empirical facts about fetal development—it is a choice about what moral status markers matter (genetic uniqueness, neurological integration, independent survival capacity, legal personhood).',
    'If no principled basis exists for choosing viability over alternatives: the reading is underdetermined by evidence alone and depends on prior normative commitments (what capacities count as morally relevant?). If such a basis exists: specify it explicitly (neurological integration enables moral agency; independent survival is marker of separable interests; etc.).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_status_boundary_alternative_framings, conceptual, 'Underdetermined choice among moral-status boundaries').

omega_variable(
    kernel_reading_contest_structure,
    'Which reading (viability, conception, birth) is currently winning the institutional contest, and does the winning reading owe its success to evidence or to power asymmetry?',
    'Examine which reading is encoded in law (varies by jurisdiction), which reading dominates medical practice (NICU resuscitation policies, informed consent protocols), and which reading appears in court decisions as ''the'' correct boundary. Compare the distribution of readings across high-power vs low-power jurisdictions and communities.',
    'If viability reading is winning because evidence supports it: the constraint''s classification stands. If viability reading is winning because state institutions prefer its regulatory clarity (regardless of evidence): the reading is a strategic equilibrium, not a truth-tracking achievement, and the beneficiary-identification (state regulatory authority) is more important than the moral claims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, empirical, 'Distribution of kernel readings across jurisdictions and institutional contexts').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(viability_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(viab_tr_t0, viability_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(viab_tr_t10, viability_reading, theater_ratio, 10, 0.46).
narrative_ontology:measurement(viab_tr_t20, viability_reading, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(viab_be_t0, viability_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(viab_be_t10, viability_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(viab_be_t20, viability_reading, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(viability_reading, identity_coordination).
narrative_ontology:affects_constraint(viability_reading, conception_reading).
narrative_ontology:affects_constraint(viability_reading, birth_reading).

% DUAL FORMULATION NOTE:
% The viability reading is one of three kernel readings of the personhood_boundary. Each reading (conception, viability, birth) constitutes a separate constraint story with its own ε value, victim/beneficiary structure, and classification type. The three constraints are linked via the kernel structure: they represent alternative readings of the same foundational question, not different observables of the same constraint. Network edges indicate which readings are upstream or downstream of others in the institutional contest. The viability reading influences both the conception reading (by providing an intermediate position that constrains how conception advocates frame their claim) and the birth reading (by providing a precedent for state authority post-viability that birth advocates must address). The three readings coexist as live positions held by different jurisdictions, courts, and moral traditions; none forecloses the others within a single integrated framework, though all three cannot be law simultaneously in any single jurisdiction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
