% ============================================================================
% CONSTRAINT STORY: personhood_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personhood_continuity_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: personhood_continuity_reading
 *   human_readable: Continuity-of-Personhood Reading of the Personhood Boundary Kernel
 *   domain: moral_philosophy/metaethics/history_of_ethics
 *
 * SUMMARY:
 *   This story instantiates the continuity reading of the personhood boundary
 *   kernel: the claim that because embryonic-to-infant development is a
 *   continuous biological process with no natural discontinuity, and
 *   infanticide is uncontroversially murder, abortion must be treated as
 *   murder by the same continuity logic, and innocent life must be
 *   categorically protected from conception onward. This is ONE reading among
 *   several live readings of the same underlying kernel (the moment/criterion
 *   at which moral status attaches). The autonomy reading and the
 *   golden-rule-consistency reading are separate constraints, generated
 *   separately, and this story does not describe, average over, or hedge
 *   against them — it presents the continuity reading's own structure
 *   cleanly, with its own ε, its own beneficiary/victim set, and its own
 *   classification.
 *
 * KEY AGENTS:
 *   - embryo_and_fetus: primary intended beneficiary of the moral claim (powerless/trapped) — the entity the reading grants standing to
 *   - pregnant_people_seeking_abortion: primary bearer of the reading's costs when enacted into law (powerless/trapped)
 *   - abortion_providers: secondary payer, professional and legal exposure (moderate/constrained)
 *   - pro_life_advocacy_organizations: agenda-setters who formalize and litigate the reading (organized/mobile)
 *   - religious_institutions_opposing_abortion: doctrinal beneficiary and co-agenda-setter (institutional/arbitrage)
 *   - legislators_and_courts: convert the reading into binding enforcement (institutional/analytical)
 *   - moral_philosophers_analytical_observers: examine the argument's logical structure without jurisdictional stake (analytical/universal)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_continuity_reading, 0.42).
domain_priors:suppression_score(personhood_continuity_reading, 0.61).
domain_priors:theater_ratio(personhood_continuity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_continuity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(personhood_continuity_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(personhood_continuity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_continuity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(personhood_continuity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_continuity_reading, tangled_rope).
narrative_ontology:human_readable(personhood_continuity_reading, "Continuity-of-Personhood Reading of the Personhood Boundary Kernel").
narrative_ontology:topic_domain(personhood_continuity_reading, "moral_philosophy/metaethics/history_of_ethics").

domain_priors:requires_active_enforcement(personhood_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_continuity_reading, '5da4c7c1-cc31-4553-b59a-0eecc15fe229').
narrative_ontology:cs_kernel_codification('5da4c7c1-cc31-4553-b59a-0eecc15fe229', distributed).
narrative_ontology:cs_authority_grounding('5da4c7c1-cc31-4553-b59a-0eecc15fe229', distributed).
narrative_ontology:cs_reading_relation('5da4c7c1-cc31-4553-b59a-0eecc15fe229', personhood_boundary_kernel__autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('5da4c7c1-cc31-4553-b59a-0eecc15fe229', personhood_boundary_kernel__golden_rule_consistency_reading, coexists_with).
narrative_ontology:cs_axiom('5da4c7c1-cc31-4553-b59a-0eecc15fe229', foundational, developmental_continuity_entails_status_equivalence).
narrative_ontology:cs_axiom_status(developmental_continuity_entails_status_equivalence, holdable).
narrative_ontology:cs_axiom_grounding('5da4c7c1-cc31-4553-b59a-0eecc15fe229', developmental_continuity_entails_status_equivalence, deontological).
narrative_ontology:cs_axiom('5da4c7c1-cc31-4553-b59a-0eecc15fe229', foundational, innocent_life_categorically_protected_regardless_of_stage).
narrative_ontology:cs_axiom_status(innocent_life_categorically_protected_regardless_of_stage, holdable).
narrative_ontology:cs_axiom_grounding('5da4c7c1-cc31-4553-b59a-0eecc15fe229', innocent_life_categorically_protected_regardless_of_stage, deontological).
narrative_ontology:cs_reference_frame('5da4c7c1-cc31-4553-b59a-0eecc15fe229', conception_as_moral_status_onset).
narrative_ontology:cs_drift_state('5da4c7c1-cc31-4553-b59a-0eecc15fe229', post_roe_overturn_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('5da4c7c1-cc31-4553-b59a-0eecc15fe229', '').
narrative_ontology:cs_kernel_id(personhood_continuity_reading, personhood_boundary_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_continuity_reading, embryo_and_fetus).
narrative_ontology:constraint_beneficiary(personhood_continuity_reading, pro_life_advocacy_organizations).
narrative_ontology:constraint_beneficiary(personhood_continuity_reading, religious_institutions_opposing_abortion).
narrative_ontology:constraint_victim(personhood_continuity_reading, pregnant_people_seeking_abortion).
narrative_ontology:constraint_victim(personhood_continuity_reading, abortion_providers).
narrative_ontology:constraint_victim(personhood_continuity_reading, people_whose_pregnancies_threaten_health).
narrative_ontology:constraint_vindicates(personhood_continuity_reading, sanctity_of_innocent_life_doctrine).
narrative_ontology:constraint_vindicates(personhood_continuity_reading, developmental_continuity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, the embryo/fetus is a rights-holder from a very early developmental point, with a moral status that does not admit gradation by stage. It cannot advocate for itself; the reading grants it standing that the law is then asked to enforce on its behalf. It has no exit from the arrangement because the arrangement is precisely about foreclosing exit from gestation.
narrative_ontology:constraint_stakeholder(personhood_continuity_reading, embryo_and_fetus, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_non_agent(personhood_continuity_reading, embryo_and_fetus).

% Bears the full weight of gestation, birth, and (in the reading's logic) potential criminal liability for terminating a pregnancy. Under this reading their autonomy claim is explicitly subordinated to the embryo's claimed personhood; where legal enforcement follows the reading, their exit options (travel, black-market procedures, self-managed abortion) are constrained or criminalized depending on jurisdiction.
narrative_ontology:constraint_stakeholder(personhood_continuity_reading, pregnant_people_seeking_abortion, payer,
    powerless, biographical, trapped, national).

% Practice or would practice abortion care; under jurisdictions adopting this reading's legal implications, providers face licensing threats, criminal prosecution, or professional exclusion. Their exit is to relocate practice to jurisdictions with a different reading, or to exit the profession's reproductive-care segment entirely.
narrative_ontology:constraint_stakeholder(personhood_continuity_reading, abortion_providers, payer,
    moderate, biographical, constrained, regional).

% Face medical situations where continuing pregnancy carries serious health risk; under a strict application of this reading's categorical protection, exceptions become contested case-by-case, and delay in care itself becomes a cost imposed by the reading's logic of near-absolute protection.
narrative_ontology:constraint_stakeholder(personhood_continuity_reading, people_whose_pregnancies_threaten_health, payer,
    powerless, immediate, trapped, national).

% Author, litigate, and legislate this reading into law; run public campaigns built on the continuity argument (embryo-to-infant developmental identity) and lobby for enforcement mechanisms including criminal statutes, waiting periods, and provider restrictions. They do not bear the reading's costs directly and can shift strategy across jurisdictions.
narrative_ontology:constraint_stakeholder(personhood_continuity_reading, pro_life_advocacy_organizations, agenda_setter,
    organized, generational, mobile, national).

% Provide doctrinal grounding and moral authority for the continuity argument, mobilize congregations, and benefit from the reading's alignment with theological commitments about ensoulment or sanctity of life from conception; operate across jurisdictions regardless of any single legal outcome.
narrative_ontology:constraint_stakeholder(personhood_continuity_reading, religious_institutions_opposing_abortion, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(personhood_continuity_reading, religious_institutions_opposing_abortion, agenda_setter).

% Adjudicate and codify whichever reading of the personhood boundary prevails in a given jurisdiction; their enforcement choices (criminal statutes, civil restrictions, judicial review) are what convert the continuity argument from a philosophical claim into a binding constraint with real victims and beneficiaries.
narrative_ontology:constraint_stakeholder(personhood_continuity_reading, legislators_and_courts, agenda_setter,
    institutional, generational, analytical, national).

% Physicians and medical associations with clinical expertise in maternal-fetal medicine are frequently not the primary drafters of the legal instruments that encode this reading; their clinical judgment about viability, health thresholds, and gestational stage is often overridden by the categorical framing the reading requires.
narrative_ontology:constraint_stakeholder(personhood_continuity_reading, medical_professionals_broadly, excluded,
    moderate, biographical, constrained, national).

% Examine the continuity argument's logical structure — whether identity and continuous development entail equivalent moral status at every point along a developmental path — without being bound by any jurisdiction's enforcement of the conclusion.
narrative_ontology:constraint_stakeholder(personhood_continuity_reading, moral_philosophers_analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(personhood_continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(personhood_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, administrable bright-line rule (full personhood from a fixed early point) that avoids the difficulty of drawing and defending a non-arbitrary gradual threshold of moral status across gestation, and coordinates legal, medical, and social treatment of pregnancy around one consistent category.
% TRANSFER_FUNCTION: Moves decisional authority over pregnancy outcomes from the pregnant person to the state/legal system, and moves the costs of gestation, childbirth, and forgone reproductive autonomy from a diffuse social preference onto the specific individual carrying the pregnancy.
% ABSENT_VOICES: Pregnant people currently seeking abortion care, and the clinicians who treat them, are frequently not centrally represented in the legislative and doctrinal bodies that formalize this reading into binding law; their lived circumstances (health risk, economic capacity, timing) enter the record largely through litigation and testimony after the rule already exists.
% DISAPPEARANCE_RATIONALE: If this reading's legal instantiations disappeared overnight, jurisdictions currently criminalizing or restricting abortion under continuity logic would see immediate expansion of legal abortion access and removal of provider liability — a major rearrangement for the payer seats. Advocacy organizations and religious institutions would experience it as moral catastrophe requiring renewed mobilization. Whether 'the world' rearranges or stays the same is genuinely disputed between the reading's proponents (who hold the underlying moral claim is a fixed fact independent of law) and its critics (who hold the constraint is a constructed legal imposition with no natural-law necessity).
% FOUNDING_PROBLEM: The problem of where to locate a non-arbitrary moral boundary between 'not yet a person' and 'a person,' given that biological development from conception to infancy is continuous with no sharp natural discontinuity — if infanticide is uncontroversially wrong, the continuity argument asks what principled reason exists to treat abortion differently.
% FOUNDING_PROBLEM_CORROBORATION: Pro-life philosophers and theologians attest the founding problem (locating a principled moral discontinuity) remains unsolved and therefore live. Secular bioethicists and philosophers outside the pro-life movement (e.g., those working in the autonomy and gradualist traditions) attest that the problem is dissolved rather than solved by treating moral status as gradual or interest-based rather than binary, and that the continuity reading persists institutionally via advocacy and doctrine rather than via unresolved philosophical necessity. No consensus corroboration exists outside the interested parties on either side.
narrative_ontology:disappearance_verdict(personhood_continuity_reading, contested).
narrative_ontology:founding_problem_status(personhood_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_continuity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-22',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(personhood_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_continuity_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_continuity_reading_tests).
:- end_tests(personhood_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) because the reading's central transfer is not economic rent but a transfer of decisional authority and bodily autonomy from the pregnant person to legal enforcement apparatus acting on behalf of the embryo's claimed status. Suppression is substantial and rising (0.40 to 0.61) because the reading's persistence in law requires increasingly active enforcement — criminal statutes, provider restrictions, cross-border travel bans in some jurisdictions — as resistance from the payer seats and clinical community grows. Theater ratio stays low and roughly flat (0.15 to 0.20): most of the apparatus (legislation, litigation, criminal prosecution) performs a real function within its own logic rather than being merely symbolic, though public-facing advocacy campaigns carry some performative share.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats (advocacy organizations, religious institutions), the arrangement is a genuine philosophical and moral coordination achievement: resolving an otherwise arbitrary line by extending existing consensus about infanticide backward along a continuous developmental path. From the payer seats (pregnant people, providers), the same arrangement operates as an imposed extraction of bodily autonomy and professional latitude, enforced by criminal law rather than persuasion. The engine's per-seat computation is expected to diverge sharply here — that divergence is exactly what a contested kernel reading should produce, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   The embryo is coded as beneficiary of the moral standing this reading grants it, though as a non-agent it collects nothing directly; the coordination benefit is captured instead by advocacy organizations and religious institutions who administer and are vindicated by the reading. Pregnant people seeking abortion and providers sit at the target end: high d, because the reading's whole legal implication is to override their autonomy claim and impose direct costs (criminal liability, forced continuation of pregnancy, professional risk). Legislators and courts are agenda-setters with analytical exit because their role is to adjudicate the reading's legal instantiation, not to bear its costs personally.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (locating a principled discontinuity in a continuous developmental process) is genuinely contested rather than resolved: proponents hold it is unsolved and therefore the reading remains necessary; critics hold that gradualist or interest-based frameworks dissolve the problem by rejecting the binary framing entirely. This is precisely the kind of contested founding-problem status the R5 genealogy interview is built to surface — the reading's proponents are its own best witnesses, and no neutral corroboration exists outside interested parties on either side.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_implies_equivalence_ambiguity,
    'Does biological developmental continuity between embryo and infant entail moral-status equivalence at every point on that continuum, or does continuity merely describe a physical process without settling which points on it carry full moral status?',
    'This is likely irreducibly conceptual: it depends on whether one accepts a threshold-free view of moral status (any non-arbitrary threshold is illegitimate, so status must be binary and early) versus a graduated or interest-based view (status can track developing capacities without requiring a single bright line). No empirical finding about fetal development settles which metaethical framework is correct.',
    'If continuity is taken to entail equivalence, the reading''s core claim holds and abortion restriction follows from accepting infanticide''s wrongness. If continuity is rejected as entailing equivalence, the reading''s central inference fails and the kernel''s boundary question must be settled by some other criterion (autonomy, interests, viability, reciprocity) — which is exactly what the sibling readings attempt.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(continuity_implies_equivalence_ambiguity, conceptual, 'Whether developmental continuity logically entails identical moral status at every developmental point.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Which of the personhood_boundary_kernel''s live readings (continuity, autonomy, golden-rule-consistency) should govern binding law, given that each is held by a substantial party as their genuine framework and none has achieved cross-framework consensus?',
    'Not resolvable by data alone; this is the committer-axis question itself. Legislative and judicial processes currently resolve it jurisdiction-by-jurisdiction via majoritarian and constitutional mechanisms rather than by philosophical adjudication, which does not settle the underlying kernel dispute, only its local legal instantiation.',
    'Different reading selections produce entirely different beneficiary/victim structures and entirely different classifications (this reading computes closer to tangled_rope/snare depending on enforcement intensity; the autonomy reading would invert much of the victim set). This is the structural reason the kernel is decomposed into separate stories rather than one story with a selection parameter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, preference, 'Which sibling reading of the personhood boundary kernel ought to govern, given genuine cross-framework disagreement.').

omega_variable(
    enforcement_intensity_variance_by_jurisdiction,
    'Does the reading''s classification shift toward snare (rather than tangled_rope) in jurisdictions where enforcement includes criminal prosecution of the pregnant person themselves, versus jurisdictions that restrict only providers?',
    'Comparative jurisdictional analysis: code enforcement targets (provider-only vs. patient-inclusive) and correlate with measured suppression and victim breadth across jurisdictions adopting this reading.',
    'Patient-inclusive criminalization would sharply raise suppression and narrow any residual coordination framing, pushing the computed type toward snare; provider-only restriction preserves more of the tangled_rope structure (a real, if contested, coordination claim coexisting with enforcement costs).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_intensity_variance_by_jurisdiction, empirical, 'Whether enforcement target (patient vs. provider) shifts the reading''s structural classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_continuity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_continuity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(pers_tr_t10, personhood_continuity_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(pers_tr_t20, personhood_continuity_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(pers_tr_t30, personhood_continuity_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(pers_tr_t40, personhood_continuity_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement(pers_tr_t50, personhood_continuity_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_continuity_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(pers_be_t10, personhood_continuity_reading, base_extractiveness, 10, 0.31).
narrative_ontology:measurement(pers_be_t20, personhood_continuity_reading, base_extractiveness, 20, 0.34).
narrative_ontology:measurement(pers_be_t30, personhood_continuity_reading, base_extractiveness, 30, 0.37).
narrative_ontology:measurement(pers_be_t40, personhood_continuity_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(pers_be_t50, personhood_continuity_reading, base_extractiveness, 50, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t0, personhood_continuity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(pers_su_t10, personhood_continuity_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(pers_su_t20, personhood_continuity_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(pers_su_t30, personhood_continuity_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(pers_su_t40, personhood_continuity_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(pers_su_t50, personhood_continuity_reading, suppression_requirement, 50, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(personhood_continuity_reading, autonomy_reading).
narrative_ontology:affects_constraint(personhood_continuity_reading, golden_rule_consistency_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of personhood_boundary_kernel. autonomy_reading and golden_rule_consistency_reading are separate constraint files with independent ε values, beneficiary/victim structures, and classifications. The continuity reading places the embryo in the victim-protected set and the pregnant person's autonomy claim as overridden; the autonomy reading inverts this priority; the golden_rule_consistency_reading substitutes a reciprocity test for the developmental-continuity test entirely. All three are linked bidirectionally via affects_constraints per the kernel decomposition rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(personhood_continuity_reading, organized, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
