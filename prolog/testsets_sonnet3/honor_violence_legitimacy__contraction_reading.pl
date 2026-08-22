% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__contraction_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: honor_violence_legitimacy__contraction_reading
 *   human_readable: Honor Code as Redefined to Exclude Violence (Contraction Reading)
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the contraction reading of the
 *   honor-violence-legitimacy kernel: the claim that dueling's decline
 *   reflects a genuine conceptual redefinition of honor itself, such that
 *   violent self-redress fell out of the legitimate honor-response set rather
 *   than merely becoming rare due to rising external costs (the drop reading)
 *   or fading through a combination of both mechanisms (the composite
 *   reading). Under this reading, the referent for extractiveness is the
 *   standing arrangement under contest as this reading sees it: the
 *   redefinition project itself, actively promoted by state, reform, and
 *   professional actors, which delegitimizes a previously legitimate elite
 *   practice and, in doing so, transfers status-adjudication authority to
 *   institutions that benefit from the new definition. The redefinition is
 *   authored here as low-moderate extraction because it is substantially a
 *   genuine coordination improvement (fewer elite deaths, more predictable
 *   adjudication) but carries a real, non-trivial cost imposed on those whose
 *   identity was constituted by the old code — hence tangled
 *   coordination-with-cost rather than pure extraction, though the claimed
 *   type here is piton: what began as an active redefinition campaign has, by
 *   the end of the interval, become mostly retrospective moral narrative
 *   (theater_ratio rising to 0.4) with little live resistance left to
 *   overcome.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__contraction_reading, 0.28).
domain_priors:suppression_score(honor_violence_legitimacy__contraction_reading, 0.35).
domain_priors:theater_ratio(honor_violence_legitimacy__contraction_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__contraction_reading, piton).
narrative_ontology:human_readable(honor_violence_legitimacy__contraction_reading, "Honor Code as Redefined to Exclude Violence (Contraction Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__contraction_reading, "historical_sociology/legal_anthropology/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__contraction_reading, '820dee1e-94d6-4e3d-aeeb-ad0dc1df417d').
narrative_ontology:cs_kernel_codification('820dee1e-94d6-4e3d-aeeb-ad0dc1df417d', distributed).
narrative_ontology:cs_authority_grounding('820dee1e-94d6-4e3d-aeeb-ad0dc1df417d', practice).
narrative_ontology:cs_interpretation_layer_present('820dee1e-94d6-4e3d-aeeb-ad0dc1df417d').
narrative_ontology:cs_reading_relation('820dee1e-94d6-4e3d-aeeb-ad0dc1df417d', honor_violence_legitimacy__drop_reading, coexists_with).
narrative_ontology:cs_reading_relation('820dee1e-94d6-4e3d-aeeb-ad0dc1df417d', honor_violence_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('820dee1e-94d6-4e3d-aeeb-ad0dc1df417d', foundational, honor_is_conceptually_severable_from_violence).
narrative_ontology:cs_axiom_status(honor_is_conceptually_severable_from_violence, holdable).
narrative_ontology:cs_axiom_grounding('820dee1e-94d6-4e3d-aeeb-ad0dc1df417d', honor_is_conceptually_severable_from_violence, conventional).
narrative_ontology:cs_axiom('820dee1e-94d6-4e3d-aeeb-ad0dc1df417d', secondary, willingness_to_duel_no_longer_constitutes_honor).
narrative_ontology:cs_axiom_status(willingness_to_duel_no_longer_constitutes_honor, holdable).
narrative_ontology:cs_axiom_grounding('820dee1e-94d6-4e3d-aeeb-ad0dc1df417d', willingness_to_duel_no_longer_constitutes_honor, conventional).
narrative_ontology:cs_reference_frame('820dee1e-94d6-4e3d-aeeb-ad0dc1df417d', aristocratic_code_duello_honor).
narrative_ontology:cs_drift_state('820dee1e-94d6-4e3d-aeeb-ad0dc1df417d', late_nineteenth_century_bourgeois_consolidation, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('820dee1e-94d6-4e3d-aeeb-ad0dc1df417d', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, bourgeois_professional_class).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, state_judicial_monopoly).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, moral_reform_societies).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, residual_aristocratic_honor_culture).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, military_officer_corps_traditionalists).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__contraction_reading, honor_is_conceptually_separable_from_violence).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__contraction_reading, state_monopoly_on_legitimate_force).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rising professionals (lawyers, physicians, merchants, civil servants) whose social standing depends on reputational and institutional credentials rather than blood or the sword. As honor is redefined around probity, credentialed competence, and civil litigation, their existing status markers become the legitimate currency of honor, and they benefit from a conceptual shift that costs them nothing since they never dueled.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, bourgeois_professional_class, beneficiary,
    organized, generational, mobile, national).

% Courts, legislatures, and prosecutors actively promote and codify the redefinition of honor to exclude violent self-redress, since dueling is a standing rival claim to the legitimate use of force. They administer libel law, courts of honor, and criminal sanctions that channel honor disputes into state-sanctioned forums, consolidating the state's monopoly on violence.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, state_judicial_monopoly, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Religious and civic reform movements campaign publicly against dueling as barbaric and irrational, producing pamphlets, sermons, and legislative lobbying. They gain moral authority and institutional standing from having their redefinition of honor adopted as the dominant cultural frame.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, moral_reform_societies, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__contraction_reading, moral_reform_societies, agenda_setter).

% Aristocratic and gentry families whose entire self-conception and claim to social precedence was historically constituted through willingness to duel. As honor is redefined to exclude violence, their inherited identity marker is delegitimized; they cannot simply exit the redefinition because their status was constituted by the very practice now excluded, and continuing to duel now marks them as reckless or criminal rather than honorable.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, residual_aristocratic_honor_culture, payer,
    powerful, biographical, identity_locked, national).

% Officers whose professional culture treated the duel as the proof of fitness to command and the remedy for insult among equals. Military codes are slower to abandon dueling than civilian law, leaving traditionalist officers caught between an institutional subculture that still tacitly permits the practice and a wider legal and social order that now treats it as archaic or criminal.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, military_officer_corps_traditionalists, payer,
    organized, biographical, constrained, national).

% Scholars examine court records, honor-code literature, and dueling statistics to assess whether the decline reflects genuine conceptual redefinition (this reading), mere practical suppression of an unchanged norm (the drop reading), or both operating together (the composite reading). Their disagreement over which reading fits the evidence is itself part of the historical record.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, legal_and_social_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Redefining honor to exclude violence solves a genuine coordination problem: it removes a standing, socially sanctioned justification for lethal private combat among elites, replacing informal violent adjudication with courts, press, and institutionalized reputational sanctions that scale better across an increasingly complex, urban, commercial society.
% TRANSFER_FUNCTION: Moves the authority to adjudicate insult and vindicate reputation from private violent contest between social equals to state courts, print media, and professional/civic institutions — shifting status-conferral power away from families and dueling codes toward the state and its credentialing bodies.
% ABSENT_VOICES: Dueling's own participants and apologists — the aristocratic and officer defenders of the code duello — are increasingly written out of respectable public discourse as their position becomes unsayable in polite or legal terms; their objection (that honor without the willingness to risk violence is empty) is structurally excluded from the redefinition's own vocabulary.
% DISAPPEARANCE_RATIONALE: If the redefinition of honor to exclude violence were reversed overnight, elite social life would reopen a live, legitimate channel for lethal private combat over reputational disputes; courts, honor societies, and professional bodies that currently monopolize reputational adjudication would lose a portion of their function to private violent settlement, and status hierarchies among elites would partially re-anchor on willingness to duel rather than on credentialed or state-sanctioned reputation.
% FOUNDING_PROBLEM: Elite society needed some legitimate, socially recognized mechanism for adjudicating insults to reputation among status-equals in a way that preserved social order and hierarchy; dueling was the inherited mechanism, but its lethality and disruption to state authority, family economics, and public order became increasingly costly as states consolidated a monopoly on legitimate force.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and contemporaneous state officials (prosecutors, legislators enacting anti-dueling statutes) attest, from outside the reform societies and professional classes who benefited, that dueling's original adjudicative function has been fully absorbed by courts and press by the late nineteenth century in most Western jurisdictions; independent quantitative studies of dueling frequency and prosecution records corroborate that the practice's rationale, not merely its incidence, had become socially unintelligible rather than merely risky.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_violence_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__contraction_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__contraction_reading_tests).
:- end_tests(honor_violence_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts low (0.12) because early redefinition efforts genuinely displaced a costly, disorder-generating practice with a workable coordination alternative, and rises modestly (to 0.28) as the redefinition hardens into orthodoxy and begins retroactively delegitimizing residual honor cultures more completely than the original coordination problem required. Theater ratio rises sharply (0.1 to 0.4) because, once dueling is functionally extinct, continued rhetorical campaigns against it (moral reform societies, textbooks, ceremonial disavowals) increasingly perform victory over a threat that no longer exists rather than doing live coordination work — consistent with the piton pattern of atrophied function maintained by institutional and narrative inertia. Accessibility collapse is high (0.82) because once the redefinition takes hold, dueling becomes not merely illegal but conceptually unavailable as an honor response — a person cannot coherently invoke honor to justify a duel within the new frame. Resistance is low-moderate (0.3) reflecting the traditionalist officer corps and residual aristocracy's fading, increasingly marginalized objections.
 *
 * DIRECTIONALITY LOGIC:
 *   The state judicial monopoly and moral reform societies are agenda-setters/beneficiaries: they actively promote and administer the redefinition and gain institutional authority from its success (d near beneficiary end). The bourgeois professional class benefits passively — their pre-existing status markers become the new honor currency without their having done anything, so they sit near full-beneficiary despite not authoring the change. Residual aristocratic honor culture and military traditionalists are the payers: their inherited identity-constituting practice is delegitimized, and their exit options are poor — aristocrats are identity_locked because status was constituted through willingness to duel, not merely regulated by it, while officers are constrained by an institutional subculture caught between old and new norms.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (adjudicating elite reputational disputes without recourse to unregulated lethal violence) is genuinely dead by the end of the interval — courts and press have fully absorbed the adjudicative function. Classifying this as piton rather than snare or rope prevents two mislabelings: treating the ongoing moral-campaign rhetoric as if it were still doing necessary coordination work (it is not — the coordination problem is solved), and treating the residual costs to traditionalist honor cultures as evidence of ongoing active extraction by an identifiable beneficiary (there is no single concentrated profiteer; the cost is diffuse and inertial, consistent with piton rather than snare).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contraction_vs_drop_evidentiary_test,
    'Is the historical decline of dueling better explained by genuine conceptual redefinition of honor (this reading) or by unchanged honor norms becoming practically costly to enact (the drop reading), or is the distinction itself unrecoverable from the evidence (the composite reading)?',
    'Close reading of contemporaneous honor-code literature, dueling manuals, and court-of-honor rulings to determine whether authors explicitly excluded violence from honor''s definition (supporting contraction) versus merely lamenting rising practical costs while still endorsing dueling in principle (supporting drop); convergent absence of either signal would support composite.',
    'If evidence supports drop instead, this constraint''s claimed conceptual-space contraction is illusory and the true mechanism is external cost escalation (dueling laws, insurance, professional risk) acting on an unchanged norm — the beneficiary/victim structure and extractiveness profile authored here would not apply; the story would need to be retired in favor of the drop_reading sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contraction_vs_drop_evidentiary_test, conceptual, 'Whether the contraction reading is empirically distinguishable from the drop reading given available historical sources.').

omega_variable(
    which_reading_is_correct_kernel_question,
    'Is there a fact of the matter about which of the three kernel readings (contraction, drop, composite) correctly describes what actually happened historically, or are all three legitimate but structurally distinct framings that different historiographical traditions adopt for different evidentiary and theoretical reasons?',
    'This is the committer-level question the kernel itself poses; it is not resolvable within a single reading''s own evidentiary frame, since each reading partly determines what counts as relevant evidence (redefinition-focused readings privilege discourse analysis; drop-focused readings privilege incident/prosecution statistics).',
    'If the composite reading is correct, treating contraction and drop as mutually exclusive stories (as this network of three constraints does) risks understating that the two mechanisms were mutually reinforcing rather than independent or competing explanations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_reading_is_correct_kernel_question, conceptual, 'Whether the three kernel readings are genuine alternatives or an artificial trichotomy imposed on an overdetermined historical process.').

omega_variable(
    identity_lock_dissolution_test,
    'Would the aristocratic honor culture''s identity_locked exit status change if a future generation''s identity were no longer constituted through the ancestral honor code (e.g., through further generational turnover)?',
    'Track whether descendants of dueling aristocratic lineages in the twentieth century treat the code duello as a live identity component or as inert historical heritage with no bearing on their self-concept.',
    'If the identity-lock dissolves generationally, the payer seat''s exit options shift from identity_locked toward mobile over a longer time horizon than this story''s interval covers, which would lower the effective extraction on later cohorts even as the constraint''s classification (piton) stays the same.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_dissolution_test, empirical, 'Whether the identity-lock on aristocratic honor culture is permanent or generationally dissolving.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__contraction_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_violence_legitimacy__contraction_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hono_tr_t20, honor_violence_legitimacy__contraction_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(hono_tr_t40, honor_violence_legitimacy__contraction_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(hono_tr_t60, honor_violence_legitimacy__contraction_reading, theater_ratio, 60, 0.3).
narrative_ontology:measurement(hono_tr_t80, honor_violence_legitimacy__contraction_reading, theater_ratio, 80, 0.36).
narrative_ontology:measurement(hono_tr_t100, honor_violence_legitimacy__contraction_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_violence_legitimacy__contraction_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(hono_be_t20, honor_violence_legitimacy__contraction_reading, base_extractiveness, 20, 0.16).
narrative_ontology:measurement(hono_be_t40, honor_violence_legitimacy__contraction_reading, base_extractiveness, 40, 0.2).
narrative_ontology:measurement(hono_be_t60, honor_violence_legitimacy__contraction_reading, base_extractiveness, 60, 0.24).
narrative_ontology:measurement(hono_be_t80, honor_violence_legitimacy__contraction_reading, base_extractiveness, 80, 0.26).
narrative_ontology:measurement(hono_be_t100, honor_violence_legitimacy__contraction_reading, base_extractiveness, 100, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(honor_violence_legitimacy__contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the honor_violence_legitimacy kernel, decomposed per the ε-invariance principle because the natural-language claim ('dueling declined because...') covers structurally distinct causal claims with different beneficiary/victim structures and different extraction profiles. contraction_reading (this story) authors ε rising 0.12→0.28 reflecting a genuine but costly conceptual redefinition; drop_reading would author a different beneficiary set (state enforcement apparatus, insurers, employers imposing practical costs) and likely a different ε trajectory reflecting suppression-by-cost rather than redefinition-by-discourse; composite_reading would need to represent both mechanisms without collapsing them into one, likely yielding an ε profile that is not simply the average of the other two. All three should remain linked via affects_constraints so contamination/coupling analysis can trace how evidence bearing on one reading's plausibility affects confidence in the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
