% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__technological_determinism, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: press_reformation_causality__technological_determinism
 *   human_readable: Printing Press as Autonomous Determinant of Reformation Success
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This story instantiates the technological_determinism reading of the
 *   press-Reformation kernel: the claim that the printing press functioned as
 *   an autonomous enabling technology whose spread of vernacular scripture
 *   made Reformation success structurally inevitable, independent of the
 *   specific strategic choices reformers, printers, and princes made. This is
 *   presented as the reading's own account — not as a neutral summary of the
 *   historical event. The theater_ratio and extractiveness rise over the
 *   interval not because the technology itself changed, but because the
 *   DETERMINIST NARRATIVE'S institutional use (as founding exemplar for media
 *   theory and confessional historiography) accumulated performative function
 *   relative to its explanatory work, as comparative counter-evidence
 *   (Counter-Reformation suppression successes) mounted without dislodging
 *   the narrative.
 *
 * KEY AGENTS:
 *   - print_capital_historians: institutional beneficiary of the determinist frame
 *   - protestant_confessional_historiography: institutional beneficiary via providentialist-adjacent narrative
 *   - media_theory_departments: institutional beneficiary via founding-exemplar status
 *   - regional_reformers_and_printers: excluded — their contingent strategic choices are flattened
 *   - counter_reformation_and_suppressed_movements: excluded — the strongest counter-evidence has no voice
 *   - comparative_historians_of_print_suppression: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__technological_determinism, 0.42).
domain_priors:suppression_score(press_reformation_causality__technological_determinism, 0.15).
domain_priors:theater_ratio(press_reformation_causality__technological_determinism, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, extractiveness, 0.42).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causality__technological_determinism, "Printing Press as Autonomous Determinant of Reformation Success").
narrative_ontology:topic_domain(press_reformation_causality__technological_determinism, "history_of_technology/religious_history/media_studies").

domain_priors:emerges_naturally(press_reformation_causality__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__technological_determinism, '2661a92b-38eb-4851-9e05-db07923eefc9').
narrative_ontology:cs_kernel_codification('2661a92b-38eb-4851-9e05-db07923eefc9', distributed).
narrative_ontology:cs_authority_grounding('2661a92b-38eb-4851-9e05-db07923eefc9', distributed).
narrative_ontology:cs_reading_relation('2661a92b-38eb-4851-9e05-db07923eefc9', press_reformation_causality__strategic_deployment, coexists_with).
narrative_ontology:cs_reading_relation('2661a92b-38eb-4851-9e05-db07923eefc9', press_reformation_causality__co_constitution, coexists_with).
narrative_ontology:cs_axiom('2661a92b-38eb-4851-9e05-db07923eefc9', foundational, technology_as_autonomous_sufficient_cause).
narrative_ontology:cs_axiom_status(technology_as_autonomous_sufficient_cause, holdable).
narrative_ontology:cs_axiom_grounding('2661a92b-38eb-4851-9e05-db07923eefc9', technology_as_autonomous_sufficient_cause, empirically_contingent).
narrative_ontology:cs_axiom('2661a92b-38eb-4851-9e05-db07923eefc9', secondary, human_strategic_choice_as_downstream_response).
narrative_ontology:cs_axiom_status(human_strategic_choice_as_downstream_response, holdable).
narrative_ontology:cs_axiom_grounding('2661a92b-38eb-4851-9e05-db07923eefc9', human_strategic_choice_as_downstream_response, empirically_contingent).
narrative_ontology:cs_reference_frame('2661a92b-38eb-4851-9e05-db07923eefc9', eisenstein_print_revolution_synthesis).
narrative_ontology:cs_drift_state('2661a92b-38eb-4851-9e05-db07923eefc9', post_comparative_censorship_scholarship, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2661a92b-38eb-4851-9e05-db07923eefc9', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__technological_determinism, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, print_capital_historians).
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, protestant_confessional_historiography).
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, media_theory_departments).
narrative_ontology:constraint_vindicates(press_reformation_causality__technological_determinism, technological_autonomy_thesis).
narrative_ontology:constraint_vindicates(press_reformation_causality__technological_determinism, print_culture_causes_reformation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars whose careers and disciplinary standing (Eisenstein-lineage media history) rest on the press-as-prime-mover narrative. A clean deterministic story is more citable, more teachable, and more resistant to case-by-case revision than an account requiring dense archival work on printer account books, patronage networks, and regional censorship regimes.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, print_capital_historians, beneficiary,
    institutional, civilizational, arbitrage, global).

% Older Protestant church-historical traditions benefit from a providentialist-adjacent narrative in which the Reformation's success was inevitable once the technological precondition existed, sidestepping harder questions about the political deals, princely self-interest, and coercive suppression of Anabaptist and radical alternatives that also shaped outcomes.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, protestant_confessional_historiography, beneficiary,
    institutional, civilizational, constrained, continental).

% Media and communications theory uses the press-determinism case as a founding exemplar (McLuhan-adjacent) for claims about how media technologies autonomously reshape societies, which grounds curricula and grant narratives extending well beyond the historical case itself.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, media_theory_departments, beneficiary,
    institutional, generational, arbitrage, global).

% The actual pamphleteers, printers, city councils, and preachers who made specific, contingent, often failed choices about what to print, where, in what dialect, and at what risk are flattened into passive conduits of an autonomous technological force under this reading — their strategic agency and the very real cases where print did NOT produce Reformation success (France, most of Italy, Spain) go unexplained.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, regional_reformers_and_printers, excluded,
    moderate, biographical, constrained, regional).

% Catholic territories with equal or greater printing capacity that suppressed vernacular scripture successfully (Spain, much of Italy, France after initial spread), and radical Reformation movements that used print but were crushed by both Protestant and Catholic authorities, are structural counter-evidence to inevitability but have no voice in a narrative organized around the press's autonomous causal power.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, counter_reformation_and_suppressed_movements, excluded,
    powerless, generational, trapped, continental).

% Scholars who study cases where printing technology existed but vernacular religious reform was successfully suppressed (Counter-Reformation Spain and Italy, Ottoman resistance to print, China's earlier printing without comparable religious rupture) can evaluate whether press availability alone predicts outcome or whether political enforcement capacity is the better predictor.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, comparative_historians_of_print_suppression, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the coordination sense — this reading names a physical/technological precondition (movable type reduces per-copy cost and enables scale) rather than a coordination mechanism among human parties. Any coordination function belongs to the sibling readings (strategic_deployment, co_constitution), not to this one.
% TRANSFER_FUNCTION: The reading itself moves interpretive authority: it transfers explanatory credit from specific human choices (which reformers wrote what, which princes protected them, which cities enforced censorship and which did not) to an impersonal technological variable, and it transfers disciplinary prestige to fields organized around technological-determinist narratives.
% ABSENT_VOICES: The printers, preachers, and city governments who made contingent, risky, reversible decisions are silenced as agents under this reading. So are the Counter-Reformation territories that had access to the same press technology and successfully suppressed vernacular scripture — their existence is the strongest available objection to inevitability, and this reading has no natural place to put them.
% DISAPPEARANCE_RATIONALE: If this reading disappeared from historiography, the Reformation as a historical event would not change — but the disciplinary self-understanding of print-culture history and media theory would rearrange substantially, since a founding causal claim used to license broader claims about technological autonomy would need replacement with contingent, actor-centered accounts. Whether 'the world' rearranges depends on whether you mean the historical past (unchanged) or the present disciplinary field built on this reading (rearranges).
% FOUNDING_PROBLEM: Early print-culture historiography (mid-20th century, especially Eisenstein's synthesis) was built to solve a real problem: prior historiography had underweighted the material and technological conditions of the Reformation, treating ideas as free-floating and print as incidental. The determinist reading corrected a genuine gap.
% FOUNDING_PROBLEM_CORROBORATION: Historians of print culture (the reading's own beneficiaries) attest the corrective was necessary and remains valid in strong form. Outside corroboration is mixed and largely critical: comparative historians of censorship (e.g. work on Counter-Reformation Spain and Italy, and on Ottoman print resistance) and social historians of the Reformation attest that the technological-precondition problem was real but has been substantially over-solved — the corrective overcorrected into an autonomy thesis the comparative record does not support, since equivalent press technology produced opposite outcomes depending on enforcement capacity and political alliance, not press availability.
narrative_ontology:disappearance_verdict(press_reformation_causality__technological_determinism, contested).
narrative_ontology:founding_problem_status(press_reformation_causality__technological_determinism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__technological_determinism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(press_reformation_causality__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__technological_determinism, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__technological_determinism_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, ExtMetricName, E),
    domain_priors:suppression_score(press_reformation_causality__technological_determinism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(press_reformation_causality__technological_determinism),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(press_reformation_causality__technological_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type is mountain because this reading's own premise is that press availability is a fixed physical/technological precondition functioning like a natural constraint — not a human choice. But the metrics are authored independently: accessibility_collapse is high (0.78) because the determinist narrative genuinely forecloses alternative causal accounts once accepted (if you believe press-as-autonomous-force, contingent political explanations look secondary by construction). Resistance is moderate (0.35) — real historiographical pushback exists from comparative and social historians. Extractiveness and theater_ratio are authored as substantial and RISING because the vindicated-proposition status of the determinist thesis increasingly serves disciplinary and confessional interests independent of its explanatory adequacy, particularly as counter-evidence from successfully-suppressed regions accumulated without revising the core claim.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (print-culture historians, confessional historiography, media theory), the press-as-mountain framing reads as settled fact underwriting further work. From the excluded seats — the actual historical actors whose strategic choices are erased, and comparative historians who can point to Counter-Reformation Spain's successful suppression with equivalent press technology — the same framing reads as a convenient causal shortcut that obscures a beneficiary structure (disciplinary prestige, confessional vindication) behind a claim of natural inevitability.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (historians, confessional traditions, media theory departments) sit at low d: they collect prestige, curricular centrality, and providentialist vindication from the constraint's operation as accepted doctrine, at negligible cost to themselves. Excluded parties (regional actors, suppressed movements) are not victims in the extraction sense — this is a claim about historiography, not a resource-extraction mechanism — so no victims are declared; but they bear a representational cost (erasure of agency, erasure of counter-evidence) that the omega variables below register.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (correcting an ideas-only historiography that ignored material/technological conditions) was real and is now substantially solved — historians broadly accept print mattered. The determinist reading's persistence in STRONG autonomous-inevitability form, well past the point the corrective was needed, is a candidate zombie-mandate pattern: mismatch between founding_problem_status (contested/largely resolved in weak form) and the reading's continued operation as though the strong form were still required. This is exactly the kind of divergence the R5 mismatch consumer is built to catch — not asserted as classification, but flagged.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    determinism_vs_strategic_agency_kernel_location,
    'Is the causal weight of the Reformation''s success located in the press as autonomous technological precondition (this reading), in reformers''/printers'' strategic deployment of it (sibling reading), or in an irreducible feedback loop between technology and agency (sibling reading)? These are not compatible causal claims about the SAME historical mechanism.',
    'Comparative case analysis across regions with equivalent press technology but divergent religious-reform outcomes (Counter-Reformation Spain/Italy vs. Germany/England/Low Countries): if outcome variance tracks political-enforcement capacity rather than press density, the determinist reading is empirically weakened relative to its siblings.',
    'If enforcement capacity (not press availability) best predicts outcome, this reading''s core premise — technology as sufficient/autonomous cause — is substantially undercut, and the beneficiary structure identified here (disciplinary prestige, confessional vindication) becomes the more plausible explanation for the reading''s persistence than its explanatory adequacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(determinism_vs_strategic_agency_kernel_location, empirical, 'Whether comparative historical evidence supports technological autonomy over strategic/co-constitutive accounts.').

omega_variable(
    mountain_classification_appropriateness,
    'Is classifying press-technology-as-cause a genuine mountain claim (a fixed physical/logical constraint with zero degrees of freedom), or is ''mountain'' itself the rhetorical move that launders a constructed historiographical convenience into the appearance of natural law?',
    'Test whether the claimed constraint would persist as stated if press access were held constant but enforcement/suppression capacity varied (the Counter-Reformation counter-cases already available). A genuine mountain claim would predict uniform outcomes wherever the technology existed; the historical record does not show this.',
    'If the mountain framing fails this test, the correct classification for the constraint ''the printing press caused the Reformation, full stop'' shifts toward a contested/constructed claim serving identifiable disciplinary and confessional beneficiaries — i.e., a false-summit pattern in the historiographical sense, structurally parallel to FSM even though this is a historiographical rather than a metric-extraction claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_classification_appropriateness, conceptual, 'Whether the determinist reading''s mountain framing is a genuine natural-constraint claim or a beneficiary-serving naturalization of a contested historical thesis.').

omega_variable(
    beneficiary_structure_visibility,
    'Does the technological-determinism reading structurally obscure its own beneficiary set (as the expected structural delta for this reading states), and if so, is that obscuring intentional disciplinary self-interest or an unintended consequence of parsimonious causal modeling?',
    'Historiographical survey of how explicitly print-culture historians and confessional historians acknowledge the disciplinary/institutional stakes riding on the determinist thesis versus treating it as neutral empirical conclusion.',
    'If beneficiaries are shown to actively resist revision proportional to their stake rather than the evidence, that supports the mandatrophy reading (mandate outlived its founding function, persists via institutional interest) over a good-faith unresolved empirical dispute.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_structure_visibility, conceptual, 'Whether the beneficiary structure obscured by mountain-framing reflects intentional or incidental disciplinary self-interest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__technological_determinism, 1450, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causality__technological_determinism, theater_ratio, 1450, 0.15).
narrative_ontology:measurement(pres_tr_t1500, press_reformation_causality__technological_determinism, theater_ratio, 1500, 0.25).
narrative_ontology:measurement(pres_tr_t1550, press_reformation_causality__technological_determinism, theater_ratio, 1550, 0.4).
narrative_ontology:measurement(pres_tr_t1600, press_reformation_causality__technological_determinism, theater_ratio, 1600, 0.5).
narrative_ontology:measurement(pres_tr_t1650, press_reformation_causality__technological_determinism, theater_ratio, 1650, 0.55).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causality__technological_determinism, base_extractiveness, 1450, 0.1).
narrative_ontology:measurement(pres_be_t1500, press_reformation_causality__technological_determinism, base_extractiveness, 1500, 0.2).
narrative_ontology:measurement(pres_be_t1550, press_reformation_causality__technological_determinism, base_extractiveness, 1550, 0.3).
narrative_ontology:measurement(pres_be_t1600, press_reformation_causality__technological_determinism, base_extractiveness, 1600, 0.38).
narrative_ontology:measurement(pres_be_t1650, press_reformation_causality__technological_determinism, base_extractiveness, 1650, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(press_reformation_causality__technological_determinism, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(press_reformation_causality__technological_determinism, press_reformation_causality__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causality__technological_determinism, press_reformation_causality__co_constitution).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the press_reformation_causality kernel. technological_determinism (this file) authors the press as autonomous mountain-like precondition; strategic_deployment authors reformers/printers as strategic agents weaponizing available technology (a rope/tangled_rope candidate with clear beneficiaries and active choices); co_constitution authors an irreducible feedback loop between technology and agency (structurally resists clean mountain/rope/snare classification, likely a distinct hybrid). Each carries its own epsilon per the ε-invariance principle — do not average across them or treat one as more 'true' than another; they are structurally distinct claims sharing a natural-language label.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
