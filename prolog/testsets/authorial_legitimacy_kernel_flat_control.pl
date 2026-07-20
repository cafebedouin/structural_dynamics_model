% ============================================================================
% CONSTRAINT STORY: authorial_legitimacy_kernel_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_authorial_legitimacy_kernel_flat_control, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: authorial_legitimacy_kernel_flat_control
 *   human_readable: Self-Authorizing Ratification Act of Constitution 2.0's Sole Author
 *   domain: constitutional_design/political_theory/sovereignty_architecture
 *
 * SUMMARY:
 *   Constitution 2.0 grounds its legitimacy not in continuity with the legal
 *   order it displaces, nor in derivation from the Sovereign Cellular Accord
 *   that inspired its architecture, but in a single self-authorizing
 *   ratification act performed by a sole author operating under an explicitly
 *   named 'benign dictator/author mode.' Sections 4 and 7.1 promise that this
 *   authorial power is designed to dissolve into the ratifying act itself —
 *   the author's exceptional power is supposed to be self-consuming, present
 *   only long enough to found the order and then extinguished by the very act
 *   of founding. This story authors that commitment FLAT: as one constraint,
 *   without decomposing into separate readings of what 'dissolve' means or
 *   who gets to certify it. The contestation is instead carried by the
 *   stakeholder seats (the drafting class and early coalition read
 *   dissolution as achieved; prior-order claimants and outside scholars read
 *   it as undemonstrated) and by omegas naming the open empirical and
 *   conceptual questions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(authorial_legitimacy_kernel_flat_control, 0.42).
domain_priors:suppression_score(authorial_legitimacy_kernel_flat_control, 0.51).
domain_priors:theater_ratio(authorial_legitimacy_kernel_flat_control, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(authorial_legitimacy_kernel_flat_control, extractiveness, 0.42).
narrative_ontology:constraint_metric(authorial_legitimacy_kernel_flat_control, suppression_requirement, 0.51).
narrative_ontology:constraint_metric(authorial_legitimacy_kernel_flat_control, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(authorial_legitimacy_kernel_flat_control, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(authorial_legitimacy_kernel_flat_control, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(authorial_legitimacy_kernel_flat_control, tangled_rope).
narrative_ontology:human_readable(authorial_legitimacy_kernel_flat_control, "Self-Authorizing Ratification Act of Constitution 2.0's Sole Author").
narrative_ontology:topic_domain(authorial_legitimacy_kernel_flat_control, "constitutional_design/political_theory/sovereignty_architecture").

domain_priors:requires_active_enforcement(authorial_legitimacy_kernel_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(authorial_legitimacy_kernel_flat_control, authorial_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(authorial_legitimacy_kernel_flat_control, founding_author).
narrative_ontology:constraint_beneficiary(authorial_legitimacy_kernel_flat_control, early_adopter_coalition).
narrative_ontology:constraint_beneficiary(authorial_legitimacy_kernel_flat_control, constitutional_drafting_class).
narrative_ontology:constraint_victim(authorial_legitimacy_kernel_flat_control, prior_legal_order_claimants).
narrative_ontology:constraint_victim(authorial_legitimacy_kernel_flat_control, sovereign_cellular_accord_signatories).
narrative_ontology:constraint_victim(authorial_legitimacy_kernel_flat_control, future_amending_generations).
narrative_ontology:constraint_vindicates(authorial_legitimacy_kernel_flat_control, self_authorizing_ratification_doctrine).
narrative_ontology:constraint_vindicates(authorial_legitimacy_kernel_flat_control, dissolving_authorship_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts and performs the ratification act under 'benign dictator/author mode,' declaring that the document's authority springs from that act alone rather than from the displaced legal order or the Accord that inspired it. Sections 4 and 7.1 formally commit this power to dissolve into the ratifying act — but the author controls the pace, sequencing, and interpretive gloss of that dissolution, and remains the sole party positioned to declare when dissolution is complete.
narrative_ontology:constraint_stakeholder(authorial_legitimacy_kernel_flat_control, founding_author, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(authorial_legitimacy_kernel_flat_control, founding_author, beneficiary).

% Political and institutional actors who backed the ratification early, securing favorable positions (drafting committee seats, transitional offices, interpretive authority) that were allocated during the author's still-active dictator/author window. Their standing depends on the legitimacy story holding — if the self-authorizing act is later read as usurpation rather than founding, their appointments lose their grounding.
narrative_ontology:constraint_stakeholder(authorial_legitimacy_kernel_flat_control, early_adopter_coalition, beneficiary,
    organized, generational, constrained, national).

% Lawyers, theorists, and drafters who built careers interpreting §4 and §7.1's dissolution clause. They gain professional authority by being the recognized experts on when and how the author's power has (or has not) dissolved — a determination with no independent adjudicator besides their own guild.
narrative_ontology:constraint_stakeholder(authorial_legitimacy_kernel_flat_control, constitutional_drafting_class, beneficiary,
    powerful, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(authorial_legitimacy_kernel_flat_control, constitutional_drafting_class, agenda_setter).

% Officials, courts, and citizens whose standing derived from the displaced legal order. The founding commitment explicitly denies their order any role in grounding the new authority, erasing their claims to continuity or compensation. They cannot appeal to the old law because the new order's legitimacy is defined precisely by NOT deriving from it.
narrative_ontology:constraint_stakeholder(authorial_legitimacy_kernel_flat_control, prior_legal_order_claimants, payer,
    moderate, biographical, trapped, national).

% Parties to the Accord that inspired Constitution 2.0's design now find their document treated as mere inspiration rather than source — their negotiated commitments and reciprocal obligations under the Accord carry no binding force on the new constitutional order. They lose leverage they believed the Accord secured, despite the document borrowing its architecture.
narrative_ontology:constraint_stakeholder(authorial_legitimacy_kernel_flat_control, sovereign_cellular_accord_signatories, payer,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(authorial_legitimacy_kernel_flat_control, sovereign_cellular_accord_signatories, excluded).

% Citizens who will live under the constitution long after the founding act, and who must accept the self-authorizing story as settled history in order to use the amendment procedures the document provides. They bear the cost of any gap between the claimed dissolution of authorial power and its actual persistence, since they inherit whatever precedent the drafting class settles on.
narrative_ontology:constraint_stakeholder(authorial_legitimacy_kernel_flat_control, future_amending_generations, payer,
    powerless, civilizational, trapped, national).

% Adjudicate disputes arising under the new constitution and must, at some point, rule on whether §7.1's dissolution has actually occurred — a question the founding commitment structurally routes back to interpreters trained within the order the commitment created.
narrative_ontology:constraint_stakeholder(authorial_legitimacy_kernel_flat_control, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(authorial_legitimacy_kernel_flat_control, founding_author).
narrative_ontology:fixing_cost_class(authorial_legitimacy_kernel_flat_control, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, non-regressive starting point for a new constitutional order — avoiding an infinite justificatory regress back through the displaced law or the Accord by locating authority in one dateable, nameable act, which lets subsequent institutions coordinate around a fixed founding moment rather than relitigating priority claims indefinitely.
% TRANSFER_FUNCTION: Moves interpretive and institutional authority away from the displaced legal order's claimants and the Accord's signatories, concentrating it first in the sole author and then, via the promised dissolution, in whichever class of interpreters (drafting class, courts, early coalition) is positioned to declare the dissolution complete on favorable terms.
% ABSENT_VOICES: Citizens and officials who operated under the displaced legal order, and the Accord's other signatories, were not parties to the ratification act and have no procedural standing to contest the founding commitment's denial of their document's grounding role — they can only object from outside a framework that pre-empts their objection by definition.
% DISAPPEARANCE_RATIONALE: The drafting class and early coalition would say the entire constitutional order rearranges — without the self-authorizing founding story, every downstream institution loses its grounding and must relitigate its origin. The prior-order claimants and Accord signatories would say comparatively little changes for them: their exclusion was already total, and removing the doctrine simply reopens questions the doctrine had closed by fiat rather than by resolution.
% FOUNDING_PROBLEM: A new constitutional order needed a legitimacy story that did not depend on continuity with an order being deliberately displaced, and did not create binding obligations to the external Accord that shaped its design — the self-authorizing ratification act solves both by locating authority nowhere but in itself.
% FOUNDING_PROBLEM_CORROBORATION: The founding author and the drafting class attest the dissolution clause is operative and the problem substantially resolved. Comparative constitutional scholars outside the drafting class, and representatives of the Accord signatories, attest that the 'dissolution' has not observably occurred in institutional practice — interpretive authority remains concentrated in the same class that benefited from the original ratification, and no external body has been given standing to certify dissolution independent of that class's own say-so.
narrative_ontology:disappearance_verdict(authorial_legitimacy_kernel_flat_control, contested).
narrative_ontology:founding_problem_status(authorial_legitimacy_kernel_flat_control, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(authorial_legitimacy_kernel_flat_control, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(authorial_legitimacy_kernel_flat_control, 'none', 1).
narrative_ontology:epsilon_provenance(authorial_legitimacy_kernel_flat_control, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(authorial_legitimacy_kernel_flat_control_tests).
:- end_tests(authorial_legitimacy_kernel_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end, rising from 0.28) because the coordination function is genuine — avoiding infinite regress through contested prior legal orders is a real problem a founding act can solve — but the benefit concentrates disproportionately in the classes positioned to interpret dissolution (drafting class, early coalition, the author's own legacy), while prior-order claimants and Accord signatories bear a flat, uncompensated loss of standing. Suppression starts higher (0.62) during the dictator/author window itself, when the sole author's power is at its most concentrated and least checked, and eases somewhat (to ~0.51) as institutions mature around the new order — but never falls to a level consistent with genuine dissolution, which is itself the diagnostic tension the theater_ratio trend (rising from 0.20 to 0.38) tracks: increasing performative invocation of §7.1's dissolution language without a correspondingly falling concentration of interpretive authority.
 *
 * PERSPECTIVAL GAP:
 *   From the founding author's and drafting class's seats, this is close to a rope: a real coordination problem (avoiding regressive legitimacy claims) solved by a mechanism that is, per its own text, self-limiting. From the prior-order claimants' and Accord signatories' seats, the same structure looks like tangled_rope shading toward snare: a coordination story used to cover an uncompensated transfer of standing, backed by continuing concentration of interpretive power that the promised dissolution has not visibly loosened. The engine should compute these divergently from the shared structural data — the claim and the metrics are authored independently here precisely so that gap is visible rather than pre-resolved.
 *
 * DIRECTIONALITY LOGIC:
 *   The founding author sits nearest full beneficiary: names the constraint into existence, controls its interpretive terms, and retains arbitrage-grade exit (the author's legacy and reputation are insulated regardless of how dissolution plays out). The drafting class and early coalition are secondary beneficiaries — they did not author the act but profit from being its recognized interpreters. Prior-order claimants and Accord signatories are structural targets: the founding commitment is defined specifically by denying their documents any grounding role, which is a direct, uncompensated transfer of legitimacy away from them. Future amending generations are targets on a civilizational horizon with trapped exit — they cannot renegotiate the founding story; they can only operate within whatever the drafting class has by then settled as its meaning.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (regress-free legitimacy for a new order) may well be solved and settled — a live coordination achievement, not merely historical. But §7.1's dissolution promise is a distinct claim from the founding problem's resolution, and it is this second claim that is contested: has the exceptional authorial power actually dissolved, or has it been laundered into the ongoing interpretive authority of the drafting class? Classifying this as tangled_rope rather than flatly as rope or snare preserves that distinction — the coordination function is real and should not be denied, but the asymmetric extraction (concentration of interpretive authority in classes that benefited from the original ratification, at the continuing cost of excluded prior claimants) is also real and should not be laundered by the coordination story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dissolution_verifiability,
    'Has the authorial power described in §7.1 actually dissolved into the ratifying act, or does it persist in displaced form as concentrated interpretive authority held by the drafting class and early coalition?',
    'Track whether interpretive rulings on constitutional meaning over successive decades increasingly diffuse across independent courts and citizen bodies, versus remaining concentrated in the original drafting lineage and its institutional heirs. A genuinely dissolved power should show diffusing interpretive authority over time; a laundered power should show persistent concentration.',
    'If dissolution is verified, the constraint reads closer to a rope or scaffold — extraordinary power was temporary and self-limiting as designed. If dissolution is not observed, the constraint is better read as tangled_rope or snare — the coordination story was real at founding but has since become cover for continuing concentrated authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dissolution_verifiability, empirical, 'Whether §7.1''s promised dissolution of authorial power is empirically observable or remains a narrative claim.').

omega_variable(
    accord_derivation_denial_coherence,
    'Is it coherent to claim the constitution is merely ''inspired by'' rather than ''derived from'' the Sovereign Cellular Accord, given how closely its architecture tracks the Accord''s structure?',
    'Comparative textual and structural analysis of Constitution 2.0 against the Accord: if core mechanisms (amendment procedures, rights architecture, institutional design) are substantially isomorphic to Accord provisions, the inspiration/derivation distinction is doing legitimacy work the structural facts do not support.',
    'If the distinction does not hold structurally, the Accord signatories'' loss of standing is harder to justify as a mere byproduct of founding a new order and reads more directly as an extraction of legitimacy that the Accord''s design work made possible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(accord_derivation_denial_coherence, conceptual, 'Whether the claimed inspiration/derivation distinction from the Accord is structurally defensible or legitimacy-preserving rhetoric.').

omega_variable(
    self_authorization_naturalness,
    'Is a self-authorizing ratification act a genuine solution to the regress problem in constitutional founding, or is ''self-authorization'' itself an unexplained primitive that merely relocates the regress rather than resolving it?',
    'Compare against other constitutional founding moments that used different legitimacy strategies (popular ratification referenda, continuity-based transitions, multilateral accession) to assess whether self-authorization by a sole named author is a distinct structural solution or a repackaging of unilateral power assertion.',
    'If self-authorization is a genuine distinct solution, the coordination-function claim in six_questions is well-grounded. If it is merely relocated regress dressed in dissolution language, the coordination story is closer to pure cover and the classification should shift toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(self_authorization_naturalness, conceptual, 'Whether self-authorization solves the founding regress problem or merely renames unilateral power assertion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(authorial_legitimacy_kernel_flat_control, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(auth_tr_t0, authorial_legitimacy_kernel_flat_control, theater_ratio, 0, 0.2).
narrative_ontology:measurement(auth_tr_t8, authorial_legitimacy_kernel_flat_control, theater_ratio, 8, 0.24).
narrative_ontology:measurement(auth_tr_t16, authorial_legitimacy_kernel_flat_control, theater_ratio, 16, 0.29).
narrative_ontology:measurement(auth_tr_t24, authorial_legitimacy_kernel_flat_control, theater_ratio, 24, 0.32).
narrative_ontology:measurement(auth_tr_t32, authorial_legitimacy_kernel_flat_control, theater_ratio, 32, 0.35).
narrative_ontology:measurement(auth_tr_t40, authorial_legitimacy_kernel_flat_control, theater_ratio, 40, 0.38).

% Extraction over time
narrative_ontology:measurement(auth_be_t0, authorial_legitimacy_kernel_flat_control, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(auth_be_t8, authorial_legitimacy_kernel_flat_control, base_extractiveness, 8, 0.33).
narrative_ontology:measurement(auth_be_t16, authorial_legitimacy_kernel_flat_control, base_extractiveness, 16, 0.37).
narrative_ontology:measurement(auth_be_t24, authorial_legitimacy_kernel_flat_control, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(auth_be_t32, authorial_legitimacy_kernel_flat_control, base_extractiveness, 32, 0.41).
narrative_ontology:measurement(auth_be_t40, authorial_legitimacy_kernel_flat_control, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(auth_su_t0, authorial_legitimacy_kernel_flat_control, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(auth_su_t8, authorial_legitimacy_kernel_flat_control, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(auth_su_t16, authorial_legitimacy_kernel_flat_control, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(auth_su_t24, authorial_legitimacy_kernel_flat_control, suppression_requirement, 24, 0.51).
narrative_ontology:measurement(auth_su_t32, authorial_legitimacy_kernel_flat_control, suppression_requirement, 32, 0.5).
narrative_ontology:measurement(auth_su_t40, authorial_legitimacy_kernel_flat_control, suppression_requirement, 40, 0.51).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(authorial_legitimacy_kernel_flat_control, enforcement_mechanism).
narrative_ontology:affects_constraint(authorial_legitimacy_kernel_flat_control, sovereign_cellular_accord_binding_force).
narrative_ontology:affects_constraint(authorial_legitimacy_kernel_flat_control, constitution_2_0_amendment_procedure).

% DUAL FORMULATION NOTE:
% This story is authored FLAT per the construction-perturbation control: it treats the founding legitimacy commitment as one constraint rather than decomposing into an originalist/interpretive reading pair. A sibling decomposition exists in principle (a 'dissolution-achieved' reading vs. a 'dissolution-pending' reading of §7.1) but is deliberately NOT authored here — the contestation between those positions is instead carried by stakeholder seat divergence and the omegas above, as the flat-construction control requires.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
