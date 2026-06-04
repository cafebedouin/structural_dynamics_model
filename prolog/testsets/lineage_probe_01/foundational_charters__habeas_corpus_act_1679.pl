% ============================================================================
% CONSTRAINT STORY: foundational_charters__habeas_corpus_act_1679
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_foundational_charters__habeas_corpus_act_1679, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: foundational_charters__habeas_corpus_act_1679
 *   human_readable: Habeas Corpus Act 1679: Procedural Remedy Against Indefinite Executive Detention
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   The Habeas Corpus Act of 1679 is one reading of the foundational charters
 *   kernel — a contested political commitment claiming to ground legitimate
 *   state authority in rule of law. This reading instantiates the principle
 *   that detention must be justified procedurally and quickly, or the
 *   detained must be released. It differs structurally from the Magna Carta
 *   reading (which asserts the king's subjection to law as a foundational
 *   principle) and the Petition of Right reading (which restates medieval
 *   liberties against modern executive encroachment). The Act creates a
 *   functional procedure where the Magna Carta made a declarative claim, and
 *   it addresses modern Crown mechanisms where the Petition addressed
 *   Stuart-era abuses. The constraint exhibits the core feature of a tangled
 *   rope: genuine coordination (judges and lawyers now have defined roles,
 *   predictable procedures replace arbitrary detention) alongside asymmetric
 *   extraction (the Crown retains the right to detain, merely requiring
 *   judicial justification; indefinite detention is eliminated but justified
 *   detention persists). The measurement trajectory shows extractiveness
 *   declining sharply from 0.68 (pre-Act indefinite detention regime) to 0.28
 *   (mature procedural habeas), while theater ratio rises from 0.05 (no
 *   procedure to perform) to 0.35 (as the writ becomes ritualized).
 *   Suppression declines from 0.88 (total silence on detention justification)
 *   to 0.62 (procedure required but with exceptions and delays).
 *
 * KEY AGENTS:
 *   - Detained Persons: Primary victim of pre-Act detention regime (powerless/trapped) — subject to indefinite detention at executive discretion; beneficiary of Act's procedure (but enforcement remains uncertain)
 *   - Crown Ministers/Gaolers: Structural extractors of indefinite detention (institutional/constrained post-Act) — previously held unchecked power to detain; Act requires them to justify detention before judges
 *   - Judges: Coordinate power center created by Act (powerful/constrained) — gain authority to review detention but become liable for writs issued and defied; experience mixed benefit and burden
 *   - Parliamentary Coalition: Institutional beneficiary (institutional/arbitrage) — asserts power to regulate detention procedure, shifts power from unilateral executive toward judicial review
 *   - Legal Practitioners: Beneficiary group (organized/mobile) — gain professional role as petitioners and advocates in habeas proceedings, economic benefit from litigation
 *   - The Civilizational Legal Tradition: Long-view observer (analytical/analytical) — sees habeas corpus as central to liberal legalism but recognizes its functional degradation over time through categorization schemes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(foundational_charters__habeas_corpus_act_1679, 0.28).
domain_priors:suppression_score(foundational_charters__habeas_corpus_act_1679, 0.62).
domain_priors:theater_ratio(foundational_charters__habeas_corpus_act_1679, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(foundational_charters__habeas_corpus_act_1679, extractiveness, 0.28).
narrative_ontology:constraint_metric(foundational_charters__habeas_corpus_act_1679, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(foundational_charters__habeas_corpus_act_1679, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(foundational_charters__habeas_corpus_act_1679, tangled_rope).
narrative_ontology:human_readable(foundational_charters__habeas_corpus_act_1679, "Habeas Corpus Act 1679: Procedural Remedy Against Indefinite Executive Detention").
narrative_ontology:topic_domain(foundational_charters__habeas_corpus_act_1679, "political/constitutional").

domain_priors:requires_active_enforcement(foundational_charters__habeas_corpus_act_1679).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(foundational_charters__habeas_corpus_act_1679, '7d61c67b-e4f8-4a9e-9a08-5755e38a5eed').
narrative_ontology:cs_kernel_codification('7d61c67b-e4f8-4a9e-9a08-5755e38a5eed', formalized).
narrative_ontology:cs_authority_grounding('7d61c67b-e4f8-4a9e-9a08-5755e38a5eed', lineage).
narrative_ontology:cs_interpretation_layer_present('7d61c67b-e4f8-4a9e-9a08-5755e38a5eed').
narrative_ontology:cs_reading_relation('7d61c67b-e4f8-4a9e-9a08-5755e38a5eed', foundational_charters__magna_carta_1215, influences).
narrative_ontology:cs_reading_relation('7d61c67b-e4f8-4a9e-9a08-5755e38a5eed', foundational_charters__petition_of_right_1628, coexists_with).
narrative_ontology:cs_axiom('7d61c67b-e4f8-4a9e-9a08-5755e38a5eed', foundational, detention_requires_justification_before_judge).
narrative_ontology:cs_axiom_status(detention_requires_justification_before_judge, holdable).
narrative_ontology:cs_axiom_grounding('7d61c67b-e4f8-4a9e-9a08-5755e38a5eed', detention_requires_justification_before_judge, deontological).
narrative_ontology:cs_axiom('7d61c67b-e4f8-4a9e-9a08-5755e38a5eed', foundational, temporal_immediacy_forecloses_indefiniteness).
narrative_ontology:cs_axiom_status(temporal_immediacy_forecloses_indefiniteness, holdable).
narrative_ontology:cs_axiom_grounding('7d61c67b-e4f8-4a9e-9a08-5755e38a5eed', temporal_immediacy_forecloses_indefiniteness, empirically_contingent).
narrative_ontology:cs_reference_frame('7d61c67b-e4f8-4a9e-9a08-5755e38a5eed', arbitrary_detention_prohibition_through_judicial_procedure).
narrative_ontology:cs_drift_state('7d61c67b-e4f8-4a9e-9a08-5755e38a5eed', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7d61c67b-e4f8-4a9e-9a08-5755e38a5eed', '2026-02-26T14:33:22Z').
narrative_ontology:cs_kernel_id(foundational_charters__habeas_corpus_act_1679, foundational_charters).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(foundational_charters__habeas_corpus_act_1679, detained_persons).
narrative_ontology:constraint_beneficiary(foundational_charters__habeas_corpus_act_1679, legal_practitioners).
narrative_ontology:constraint_victim(foundational_charters__habeas_corpus_act_1679, executive_detention_power).
narrative_ontology:constraint_victim(foundational_charters__habeas_corpus_act_1679, crown_ministers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE DETAINED PERSON (SNARE) — Before habeas corpus, indefinite detention without trial is a pure extraction mechanism: the prisoner has zero exit options, zero recourse, zero timeline. The Crown extracts loyalty through terror and arbitrary power. The Act creates a procedure but does not eliminate the underlying power asymmetry — the judge must rule within days, but execution of the writ depends on crown compliance, which is structurally uncertain. From the prisoner's perspective, habeas corpus is a rope (hope of remedy) but experienced as a snare because enforcement remains at crown sufferance.
constraint_indexing:constraint_classification(foundational_charters__habeas_corpus_act_1679, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE JUDGE (TANGLED ROPE) — Judges benefit from the Act's assertion that they have authority to review detention — it elevates the judiciary as a coordinate power. But judges are also constrained: they must act quickly (within days), they operate within a framework of royal prerogative still contested, and enforcement of writs issued against ministers remains uncertain. The Act creates genuine coordination (judges and lawyers now have a defined role) alongside asymmetric extraction (judges are now responsible for legitimizing or rejecting crown detention, a burden and a power). Judges experience both benefit (authority, role definition) and cost (liability if writs are defied, political pressure from executive).
constraint_indexing:constraint_classification(foundational_charters__habeas_corpus_act_1679, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE PARLIAMENTARY COALITION (ROPE) — Parliament (the institutional beneficiary) benefits directly from the Act: it asserts parliamentary authority over detention procedures and shifts power from unilateral executive prerogative toward judicial review. The coalition has exit options (they can revoke the Act, amend it, enforce it differentially by faction) and high power. The Act is a coordination mechanism from their perspective: it establishes a shared framework for legitimate detention that reduces unpredictable executive action. Parliament coordinates with the judiciary through the Act's procedural requirements. Low extraction because the beneficiary has power and can exit.
constraint_indexing:constraint_classification(foundational_charters__habeas_corpus_act_1679, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE CROWN EXECUTIVE (SNARE) — From the executive's immediate perspective, the Act is a snare that constrains their power: they must now justify detention before a judge, quickly, or release the prisoner. The executive loses the extraction mechanism of indefinite detention (extracting loyalty through terror and uncertainty). However, this is experienced as a Snare FROM THE EXECUTIVE'S POWER, not FROM THE PRISONER'S FREEDOM — the executive is the target of the constraint, not the beneficiary. The executive can still detain (the Act does not eliminate detention, only requires procedure), so the extraction is constrained but not eliminated. From the executive's immediate perspective, the Act appears as a binding constraint (snare) that sacrifices unchecked power for procedural legitimacy.
constraint_indexing:constraint_classification(foundational_charters__habeas_corpus_act_1679, snare,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE CIVILIZATIONAL LEGAL TRADITION (PITON) — Over centuries, habeas corpus becomes a central ritual of liberal legalism: the writ persists as symbol and procedure, but its actual function (preventing indefinite detention) has been substantially commodified and circumvented. Modern detention regimes (administrative detention, national security exceptions, immigration holds) evade habeas corpus through re-categorization (not 'detention' but 'administrative custody', not 'criminal' but 'material witness', not 'indefinite' but '90-day review cycles'). The writ persists through institutional inertia and ideological attachment to the narrative of 'habeas corpus protecting liberty,' but its functional reach has degraded. The theater ratio is high (courts issue writs, executives comply with procedural forms while the actual detention power persists through classification schemes). This is piton: a once-functional constraint (rope or even mountain from the 1679-1800 perspective) that has been substantially eroded while the form persists.
constraint_indexing:constraint_classification(foundational_charters__habeas_corpus_act_1679, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: THE NATURAL LAW PHILOSOPHICAL TRADITION (MOUNTAIN) — From the perspective of natural law philosophy (Hobbes, Locke, the entire liberal tradition), habeas corpus is grounded in an immutable principle: every person has a natural right to bodily liberty and freedom from arbitrary detention. The writ is not a contingent procedural innovation but an instantiation of a law of nature — the right to petition for freedom from unlawful restraint. From this perspective, habeas corpus appears as a mountain: it is the codification of a natural law that precedes and supersedes the state. The writ's force derives from natural law, not from state permission. However, this perspective is vulnerable to false summit detection: the actual enforcement of the Act depends entirely on state machinery, judges appointed by the state, and crown compliance. The 'natural law' framing naturalizes what is actually a contingent political victory.
constraint_indexing:constraint_classification(foundational_charters__habeas_corpus_act_1679, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(foundational_charters__habeas_corpus_act_1679_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(foundational_charters__habeas_corpus_act_1679, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(foundational_charters__habeas_corpus_act_1679, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(foundational_charters__habeas_corpus_act_1679, TR),
    TR >= 0.70.

:- end_tests(foundational_charters__habeas_corpus_act_1679_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Moderate-low. The Act suppresses the extraction mechanism of indefinite detention by requiring judicial justification and time-bound procedures. However, extractiveness is not zero because: (1) the Crown retains the right to detain persons it can justify before a judge, (2) procedural delays can recreate indefiniteness within the habeas process itself, (3) prerogative exceptions (national security, raison d'état) may permit Crown to circumvent the Act, and (4) access to habeas remedy is stratified by class and legal capacity. The value reflects that the Act is a genuine constraint on executive power but not a complete elimination of detention authority. Suppression (0.62): Moderate-high. Significant barriers remain: (1) prerogative claims can override habeas requirements, (2) procedural complexity may delay judgment, (3) judges may be politically captured or pressured, (4) the definition of 'lawful cause' for detention remains contested, (5) access is limited to those who can petition (literacy, property, urban proximity to courts). The Act eliminates the total silence on detention justification (suppression was near 1.0 pre-Act) but substantial suppression persists. Theater ratio (0.35): Moderate. The Act creates a genuine procedure (petitions, hearings, judicial review) that is functionally necessary to determine whether detention is lawful. However, ritualism emerges as the writ becomes standardized and judges develop consistent practices of deferring to executive claims of lawful cause. Over time, theater rises as the writ becomes ceremonial — 'of course the Crown has lawful cause, we will issue the writ and the Crown will comply with procedure, the prisoner is released or returned to custody with formal justification.' The low starting theater (0.05) reflects that pre-Act detention had zero procedure; the rising trajectory shows how procedure, once formalized, acquires performative elements.
 *
 * PERSPECTIVAL GAP:
 *   The Act produces radically different classifications from different positions. From the detained person's perspective, the Act appears as a snare (procedure exists but enforcement is uncertain, and the underlying power asymmetry persists). From the judge's perspective, it is a tangled rope (genuine authority and coordination, but with constraints and political pressure). From Parliament's perspective, it is a rope (pure coordination shift in power without significant constraint on Parliament's own freedom). From the Crown executive's immediate perspective, it is a snare (constraints on prerogative power). From the civilizational legal tradition, it is a mountain (natural law grounding), vulnerable to false summit detection because the grounding is political and contingent, not natural. From the long historical view, it degrades to piton (ritualized, functionally eroded through categorization schemes). The perspectival gap reveals how a single structural fact (procedural habeas available for judicial review of detention) is experienced differently depending on power, exit options, and time horizon.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation: Detained persons (victims, trapped, powerless) experience high d toward 1.0 — maximum extraction relative to their position. Crown ministers (beneficiaries in the sense of retaining detention power, constrained post-Act) experience d around 0.35-0.45 — beneficiaries of the general power structure but targets of the specific constraint. Judges (both beneficiary of authority and victim of responsibility, powerful, constrained) experience d around 0.50 — symmetric. Parliament (beneficiary, institutional, arbitrage exit) experiences d around 0.15 — low extraction due to high power and exit options. The engine derives these from beneficiary/victim declarations and exit options; no override needed as the structural relationships are clear.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the Act is genuinely a tangled rope: it contains both a coordination function (establishing predictable procedure for detention review) and asymmetric extraction (the Crown retains detention power, merely constrained to justify it). The confusion arises from comparing the Act to pre-Act indefinite detention (which was pure snare) — from that comparison, the Act appears to eliminate extraction. But comparing the Act to an ideal of complete freedom from detention (which the Act does not provide), it still permits extraction. The Act is neither pure coordination (rope) nor pure extraction (snare); it is hybrid. The mandatrophy resolves by accepting that a real reduction in extraction (from 0.68 to 0.28) is consistent with the tangled rope classification: some extraction suppressed, some extraction permitted through procedure, genuine coordination mechanism established, genuine asymmetry persists. The false summit risk (from the natural law perspective) is that habeas corpus is presented as grounding in natural law rather than in political victory and institutional power balance — a victory that is reversible and historically contingent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_mechanism_uncertainty,
    'What happens when a judge issues a habeas corpus writ and the Crown refuses to comply? Does the Act create enforceable obligation or merely advisory procedure?',
    'Historical analysis of cases where writs were issued and defied; documentation of judicial and parliamentary responses; examination of the Act''s specific language regarding contempt and enforcement.',
    'If genuinely enforceable: Act is Rope (coordination with real teeth). If advisory: Act is Piton (performative ritual). If enforcement depends on factional politics: Act is Tangled Rope (asymmetric power, genuine but unstable coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_mechanism_uncertainty, empirical, 'Whether habeas corpus writs are enforceable against executive defiance').

omega_variable(
    prerogative_boundary_contest,
    'Does the Act foreclose the Crown''s ability to claim national security or raison d''état exceptions to the habeas requirement? Or does it merely shift the burden to the Crown to articulate the exception?',
    'Textual analysis of the Act''s language; historical documentation of executive claims of prerogative exceptions; comparison with Magna Carta and Petition of Right language on exceptions.',
    'If prerogative exceptions are closed: Act forecloses executive discretion (Rope or Mountain, depending on naturality view). If prerogative exceptions remain: Act creates procedure but leaves core power intact (Tangled Rope or Snare from prisoner''s perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prerogative_boundary_contest, conceptual, 'Whether habeas corpus forecloses or merely constrains prerogative detention exceptions').

omega_variable(
    magna_carta_relationship,
    'Is habeas corpus an instantiation of Magna Carta''s principle that no one may be imprisoned without lawful judgment, or a distinct structural innovation that changes the mechanism of enforcement?',
    'Textual comparison: does the Act appeal to Magna Carta as precedent (instantiation) or propose new procedure (innovation)? Historical evidence of how contemporaries understood the relationship.',
    'If instantiation: the Act merely codifies what Magna Carta already required (suggests reading_relation: influences rather than forecloses; axiom: procedural_acceleration_of_charter_principle). If innovation: the Act introduces genuinely new checking mechanism (suggests Axiom: executive_justification_before_detention forecloses the prerogative silence that Magna Carta permitted).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(magna_carta_relationship, conceptual, 'Whether habeas corpus instantiates Magna Carta or introduces structural innovation').

omega_variable(
    class_and_capacity_access,
    'Who can actually petition for habeas corpus? Does the Act extend to all detained persons or only to those with legal capacity (men of property, literate, urban, able to pay counsel)?',
    'Historical documentation of habeas petitions: demographics of petitioners, geographic distribution, correlation with class status. Archive analysis of denied or dismissed petitions by reason.',
    'If universal access: Act is Rope or Mountain (protection applies broadly). If access is class-stratified: Act is Tangled Rope or Snare from the perspective of excluded groups (benefit to the powerful, extraction from the powerless). High class-stratification suggests the Act creates a mechanism for elites to challenge detention while leaving mass detention (of poor, colonized, enslaved persons) unprotected.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(class_and_capacity_access, empirical, 'Actual access to habeas remedy as stratified by class, property, and capacity').

omega_variable(
    temporal_acceleration_mechanism,
    'What is the actual timeline between petition, hearing, and judgment? Does the ''quickly'' requirement produce timely judgment, or do procedural delays (jurisdiction questions, standing issues, evidentiary disputes) recreate indefinite detention within the habeas process itself?',
    'Statistical analysis of habeas cases: median time from petition to judgment; distribution of outcomes by time elapsed; correlation between procedural complexity and delay; comparison with non-habeas detention timelines.',
    'If timely: Act suppresses indefinite detention (extractiveness low, classification: Rope). If delays recreate indefiniteness: procedural remedy becomes procedural trap (extractiveness medium-high, classification: Tangled Rope or Piton, depending on whether delays are systematic or incidental).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(temporal_acceleration_mechanism, empirical, 'Actual timelines for habeas remedy execution and procedural delay patterns').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(foundational_charters__habeas_corpus_act_1679, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(habeas_theater_pre_act, foundational_charters__habeas_corpus_act_1679, theater_ratio, 0, 0.05).
narrative_ontology:measurement(habeas_theater_early, foundational_charters__habeas_corpus_act_1679, theater_ratio, 5, 0.25).
narrative_ontology:measurement(habeas_theater_mature, foundational_charters__habeas_corpus_act_1679, theater_ratio, 15, 0.35).

% Extraction over time
narrative_ontology:measurement(habeas_extractiveness_pre_act, foundational_charters__habeas_corpus_act_1679, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(habeas_extractiveness_early_enforcement, foundational_charters__habeas_corpus_act_1679, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(habeas_extractiveness_mature, foundational_charters__habeas_corpus_act_1679, base_extractiveness, 15, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(habeas_suppression_pre_act, foundational_charters__habeas_corpus_act_1679, suppression_requirement, 0, 0.88).
narrative_ontology:measurement(habeas_suppression_early, foundational_charters__habeas_corpus_act_1679, suppression_requirement, 5, 0.72).
narrative_ontology:measurement(habeas_suppression_mature, foundational_charters__habeas_corpus_act_1679, suppression_requirement, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(foundational_charters__habeas_corpus_act_1679, enforcement_mechanism).
narrative_ontology:affects_constraint(foundational_charters__habeas_corpus_act_1679, foundational_charters__magna_carta_1215).
narrative_ontology:affects_constraint(foundational_charters__habeas_corpus_act_1679, foundational_charters__petition_of_right_1628).

% DUAL FORMULATION NOTE:
% The foundational charters kernel family contains three structurally distinct readings: Magna Carta (declarative principle that the king is under law), Petition of Right (restatement of medieval liberties against modern abuses), and Habeas Corpus Act (procedural mechanism for detention review). Each reading has its own epsilon: Magna Carta is a mountain (emerges as natural law from the legal tradition); Petition of Right is a tangled rope (restates old liberties in new form, creating coordination and asymmetric extraction); Habeas Corpus Act is a tangled rope (creates procedure for detention review with coordination and residual extraction). The three stories are linked because each reading interprets the same kernel differently and creates structural downstream pressure on the others — the procedural specificity of habeas corpus changes what 'subjection to law' means concretely, which influences how Magna Carta is read and enforced.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(foundational_charters__habeas_corpus_act_1679, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
