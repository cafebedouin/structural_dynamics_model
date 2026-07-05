% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__monarchical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__monarchical_reading, []).

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
 *   constraint_id: sovereign_legitimacy__monarchical_reading
 *   human_readable: Monarchical Reading of Sovereign Legitimacy (Divine Right / Bloodline Continuity)
 *   domain: political_philosophy/constitutional_theory
 *
 * SUMMARY:
 *   This story instantiates the monarchical reading of the
 *   sovereign_legitimacy kernel: authority is held to flow downward from a
 *   sovereign whose right is established by bloodline continuity, tradition,
 *   and divine sanction, rather than upward from popular consent.
 *   Historically, this reading solved a real coordination problem (settling
 *   succession without repeated violent contest) but its persistence over
 *   centuries increasingly reflects the entrenched interest of the class it
 *   seats — the ruling house, titled aristocracy, and established clergy — in
 *   blocking rival legitimacy claims (elective, popular, meritocratic) that
 *   would displace them. The theater ratio rises over the measured interval
 *   as coronation ritual, genealogical record-keeping, and doctrinal ceremony
 *   increasingly substitute for any live contest about whether the
 *   arrangement still solves a real problem, even as suppression of
 *   alternative claims (heterodox succession theories, popular assemblies,
 *   republican agitation) remains persistently high throughout.
 *
 * KEY AGENTS:
 *   - hereditary_ruling_class: agenda_setter/beneficiary (institutional/arbitrage) — sets succession law and collects rents
 *   - aristocratic_hierarchy: beneficiary/agenda_setter (organized/constrained) — regional delegated authority under the same bloodline logic
 *   - established_clergy: beneficiary (institutional/constrained) — supplies divine sanction, receives land and exemption
 *   - excluded_subjects: payer (powerless/trapped) — bears taxation and conscription with no participatory forum
 *   - commoner_political_aspirants: excluded (powerless/trapped) — categorically barred from office by birth
 *   - collateral_bloodline_rivals: payer/excluded (moderate/constrained) — contest succession through the doctrine's chronic failure mode, civil war
 *   - constitutional_theorists: observer (analytical/analytical) — traces comparative structure across kernel readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, 0.71).
domain_priors:suppression_score(sovereign_legitimacy__monarchical_reading, 0.82).
domain_priors:theater_ratio(sovereign_legitimacy__monarchical_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__monarchical_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__monarchical_reading, "Monarchical Reading of Sovereign Legitimacy (Divine Right / Bloodline Continuity)").
narrative_ontology:topic_domain(sovereign_legitimacy__monarchical_reading, "political_philosophy/constitutional_theory").

domain_priors:requires_active_enforcement(sovereign_legitimacy__monarchical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__monarchical_reading, 'd73d4fce-8643-428a-b004-5bae797d191c').
narrative_ontology:cs_kernel_codification('d73d4fce-8643-428a-b004-5bae797d191c', distributed).
narrative_ontology:cs_authority_grounding('d73d4fce-8643-428a-b004-5bae797d191c', lineage).
narrative_ontology:cs_interpretation_layer_present('d73d4fce-8643-428a-b004-5bae797d191c').
narrative_ontology:cs_reading_relation('d73d4fce-8643-428a-b004-5bae797d191c', sovereign_legitimacy__republican_reading, forecloses).
narrative_ontology:cs_reading_relation('d73d4fce-8643-428a-b004-5bae797d191c', sovereign_legitimacy__constitutional_hybrid_reading, influences).
narrative_ontology:cs_axiom('d73d4fce-8643-428a-b004-5bae797d191c', foundational, authority_descends_from_sovereign_bloodline).
narrative_ontology:cs_axiom_status(authority_descends_from_sovereign_bloodline, holdable).
narrative_ontology:cs_axiom_grounding('d73d4fce-8643-428a-b004-5bae797d191c', authority_descends_from_sovereign_bloodline, theological).
narrative_ontology:cs_axiom('d73d4fce-8643-428a-b004-5bae797d191c', secondary, ritual_continuity_constitutes_legitimacy).
narrative_ontology:cs_axiom_status(ritual_continuity_constitutes_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('d73d4fce-8643-428a-b004-5bae797d191c', ritual_continuity_constitutes_legitimacy, conventional).
narrative_ontology:cs_reference_frame('d73d4fce-8643-428a-b004-5bae797d191c', divine_right_hereditary_succession).
narrative_ontology:cs_drift_state('d73d4fce-8643-428a-b004-5bae797d191c', contemporary_constitutional_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('d73d4fce-8643-428a-b004-5bae797d191c', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, hereditary_ruling_class).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, aristocratic_hierarchy).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, established_clergy).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, excluded_subjects).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, commoner_political_aspirants).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, collateral_bloodline_rivals).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__monarchical_reading, divine_right_doctrine).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__monarchical_reading, bloodline_continuity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Occupies the throne and the offices tied to it by birth. Sets succession law, appoints regional administrators from within the bloodline and allied houses, and enforces the doctrine that authority descends rather than ascends. Collects tribute, land rents, and judicial fees that flow through the crown's apparatus. Can renegotiate succession terms in its own favor; cannot be displaced by ordinary political action.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, hereditary_ruling_class, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__monarchical_reading, hereditary_ruling_class, beneficiary).

% Holds regional authority as delegated extensions of the sovereign's inherited right, in exchange for military and administrative service. Benefits from the same bloodline logic that legitimizes the crown; their own titles are hereditary. Depend on continued royal recognition and would lose standing if the legitimacy claim collapsed into an elective or popular model.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, aristocratic_hierarchy, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__monarchical_reading, aristocratic_hierarchy, agenda_setter).

% Performs the coronation rites, anointment, and doctrinal affirmation that convert bloodline into divinely sanctioned rule. In exchange, receives land grants, tax exemption, and protected institutional status. Their theological authority is intertwined with the crown's; a legitimacy model that dropped divine sanction would strip their gatekeeping function.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, established_clergy, beneficiary,
    institutional, civilizational, constrained, national).

% Bear taxation, conscription, and judicial subordination under a legitimacy claim they had no part in constituting and cannot revise. Emigration is the only exit and is costly, restricted, or unavailable to most; political voice is structurally foreclosed because participation itself would concede that authority could flow upward.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, excluded_subjects, payer,
    powerless, biographical, trapped, national).

% Possess ability, ambition, or local following but no bloodline claim, and are therefore categorically barred from the offices the doctrine reserves for hereditary right. Would argue for merit- or consent-based access to authority; their objection has no forum because the doctrine defines the forum's membership.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, commoner_political_aspirants, excluded,
    powerless, biographical, trapped, national).

% Hold plausible hereditary claims that the current succession rule does not recognize or ranks below the incumbent line. Cannot simply exit the framework — their entire claim to relevance depends on the same bloodline logic that currently excludes them — so they either submit or contest succession by force, the doctrine's chronic failure mode.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, collateral_bloodline_rivals, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__monarchical_reading, collateral_bloodline_rivals, excluded).

% Study the monarchical legitimacy claim comparatively against republican and hybrid readings, tracing how divine sanction and bloodline continuity function as suppression mechanisms against rival legitimacy claims rather than as free-standing metaphysical facts.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, constitutional_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sovereign_legitimacy__monarchical_reading, hereditary_ruling_class).
narrative_ontology:fixing_cost_class(sovereign_legitimacy__monarchical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a non-negotiable, pre-political answer to 'who rules' that forecloses succession disputes at every generational transition without requiring fresh political contestation each time — in principle reducing civil conflict relative to open contests for power.
% TRANSFER_FUNCTION: Moves tribute, land rents, judicial fees, conscripted labor, and political voice from the general population and non-favored bloodlines to the hereditary ruling house, the titled aristocracy, and the clergy that legitimizes the arrangement.
% ABSENT_VOICES: Excluded subjects and commoner aspirants have no forum in which to contest the legitimacy claim itself, because participation in that forum is defined as a hereditary privilege; collateral bloodline claimants are heard only through the extra-legal channel of succession contest and civil war.
% DISAPPEARANCE_RATIONALE: If the doctrine of inherited, divinely sanctioned authority vanished overnight, the offices, land tenures, tax exemptions, and judicial structures built on bloodline continuity would need an entirely different justificatory basis (election, appointment by merit, popular ratification) and very likely a different set of occupants — the aristocracy and clergy's institutional position is not separable from the legitimacy claim that seats them.
% FOUNDING_PROBLEM: Pre-state and early-state societies faced recurring, violent contests over who would hold coercive power at each succession; the doctrine offered a fixed, non-negotiable rule (bloodline plus ritual sanction) intended to settle the question before it could be fought over.
% FOUNDING_PROBLEM_CORROBORATION: The hereditary ruling class and clergy attest the problem remains live — that without a fixed succession principle, societies revert to violent contests for power. Constitutional theorists and comparative historians, external to the beneficiary set, attest that the succession-stability problem is real but is solved at least as well by codified constitutional succession rules without bloodline or divine-sanction premises, and that the doctrine's actual function has shifted from conflict-prevention to entrenchment of a specific hereditary class's claim on rents and office.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__monarchical_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__monarchical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__monarchical_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sovereign_legitimacy__monarchical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__monarchical_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__monarchical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sovereign_legitimacy__monarchical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71) because the doctrine channels tribute, rents, judicial fees, and conscripted labor toward the hereditary class and its allies with no participatory check. Suppression is authored higher still (0.82) because the doctrine's stability depends on foreclosing rival legitimacy claims (elective succession, popular assembly, meritocratic office) as categorically illegitimate rather than merely unpopular — the suppression is definitional to the doctrine, not incidental enforcement. Theater ratio rises across the interval (0.20 to 0.48) as coronation ritual and genealogical proof increasingly substitute for functional conflict-prevention, particularly once written constitutional succession rules become available elsewhere as a lower-suppression alternative, making the retained bloodline/divine-sanction apparatus increasingly performative relative to its stated function.
 *
 * DIRECTIONALITY LOGIC:
 *   The hereditary ruling class sits at the full-beneficiary end: it authors the rules that seat it and collects directly. The aristocracy and clergy are secondary beneficiaries whose institutional position is derivative of the same doctrine, giving them constrained but real exit (they could, in principle, defect to a rival legitimacy framework, but doing so destroys the basis of their own standing). Excluded subjects and commoner aspirants sit at the full-target end: trapped exit, no participatory forum, extraction with no compensating benefit. Collateral bloodline rivals are a distinct case: they are excluded from THIS succession line but remain committed to the doctrine's premises, so their remedy is capture of the same structure (contested succession) rather than exit to a different legitimacy claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (violent, repeated contests over coercive power at succession) was real and the doctrine's fixed-rule solution had genuine coordination value in its origin period. The mandatrophy question is whether that problem remains live in a form only bloodline-plus-ritual can solve, or whether codified constitutional succession (the hybrid reading) now solves the same coordination problem with far less suppression and narrower beneficiary concentration. Authoring this as tangled_rope (rather than snare) preserves the founding coordination function in the record while still naming the asymmetric extraction and required active enforcement — a pure snare framing would erase the genuine historical problem the doctrine solved; a pure rope framing would erase the excluded_subjects and collateral_bloodline_rivals victim classes the doctrine still produces.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_sanction_naturalness,
    'Is divine sanction of the bloodline a genuine metaphysical fact this reading correctly identifies, or a constructed doctrine whose main function is to make the hereditary ruling class''s authority appear pre-political and therefore uncontestable?',
    'Comparative historical analysis of how divine-sanction doctrines emerge, are revised by the same ruling houses when politically convenient (e.g., reinterpretation of succession law to favor a preferred heir), and are abandoned when the ruling house loses power — a doctrine that flexes with the interests of its beneficiaries is evidence of construction rather than discovery.',
    'If constructed, the coordination story (fixed succession prevents violence) is real but separable from the divine-sanction cover story, meaning the doctrine''s suppression of rival legitimacy claims is doing more extractive work than coordinating work.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_sanction_naturalness, conceptual, 'Whether divine sanction is discovered metaphysical fact or constructed legitimation device.').

omega_variable(
    succession_stability_tradeoff,
    'Does bloodline succession actually reduce violent contests for power relative to alternative fixed-rule mechanisms (codified constitutional succession, sortition, term-limited election), or does it merely relocate the violence to succession crises and collateral-line contests?',
    'Comparative frequency and severity analysis of succession-related civil conflict under monarchical versus codified-constitutional and electoral systems across comparable state capacities.',
    'If bloodline succession does not outperform codified alternatives on stability, the doctrine''s primary claimed coordination benefit collapses, strengthening a snare-leaning reclassification; if it does outperform under specific conditions (weak state institutions, low literacy, contested territorial boundaries), the tangled_rope classification with a genuine but narrowing coordination function is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(succession_stability_tradeoff, empirical, 'Whether hereditary succession genuinely reduces power-transition violence relative to alternatives.').

omega_variable(
    kernel_reading_foreclosure_scope,
    'Does the monarchical reading''s downward-flow premise foreclose the republican reading''s upward-flow premise only within a single legal-political framework, or does historical practice show societies holding both premises simultaneously in different domains (e.g., ceremonial monarchy plus elected government)?',
    'Examine constitutional_hybrid_reading as the empirical test case: if it is coherent and stable in practice, the foreclosure between monarchical and republican premises is framework-relative, not absolute.',
    'Affects whether the reading_relations edge to constitutional_hybrid_reading should be read as influences (this reading''s ritual apparatus creates downstream pressure on the hybrid''s ceremonial component) rather than forecloses — the edge to republican_reading remains forecloses because no single framework holds both directional premises for the same authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_scope, conceptual, 'Scope of logical foreclosure between directional-flow premises across kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__monarchical_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereign_legitimacy__monarchical_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sove_tr_t20, sovereign_legitimacy__monarchical_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(sove_tr_t40, sovereign_legitimacy__monarchical_reading, theater_ratio, 40, 0.34).
narrative_ontology:measurement(sove_tr_t60, sovereign_legitimacy__monarchical_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(sove_tr_t80, sovereign_legitimacy__monarchical_reading, theater_ratio, 80, 0.45).
narrative_ontology:measurement(sove_tr_t100, sovereign_legitimacy__monarchical_reading, theater_ratio, 100, 0.48).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereign_legitimacy__monarchical_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(sove_be_t20, sovereign_legitimacy__monarchical_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(sove_be_t40, sovereign_legitimacy__monarchical_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(sove_be_t60, sovereign_legitimacy__monarchical_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(sove_be_t80, sovereign_legitimacy__monarchical_reading, base_extractiveness, 80, 0.7).
narrative_ontology:measurement(sove_be_t100, sovereign_legitimacy__monarchical_reading, base_extractiveness, 100, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t0, sovereign_legitimacy__monarchical_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(sove_su_t20, sovereign_legitimacy__monarchical_reading, suppression_requirement, 20, 0.77).
narrative_ontology:measurement(sove_su_t40, sovereign_legitimacy__monarchical_reading, suppression_requirement, 40, 0.79).
narrative_ontology:measurement(sove_su_t60, sovereign_legitimacy__monarchical_reading, suppression_requirement, 60, 0.8).
narrative_ontology:measurement(sove_su_t80, sovereign_legitimacy__monarchical_reading, suppression_requirement, 80, 0.81).
narrative_ontology:measurement(sove_su_t100, sovereign_legitimacy__monarchical_reading, suppression_requirement, 100, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__monarchical_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(sovereign_legitimacy__monarchical_reading, 0.1).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, republican_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, constitutional_hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the sovereign_legitimacy kernel. monarchical_reading and republican_reading assert directly contradictory premises about the direction authority flows and cannot coexist within a single legal-political framework (forecloses). constitutional_hybrid_reading retains a ceremonial residue of this reading (inherited symbolic authority) while adopting the republican reading's premise for political authority proper, making it structurally downstream of both — this reading exerts influence on the hybrid reading's ceremonial component without foreclosing it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
