% ============================================================================
% CONSTRAINT STORY: animal_status__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__welfare_reading, []).

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
 *   constraint_id: animal_status__welfare_reading
 *   human_readable: Sentience-Constrained Use Arrangement (Welfare Reading of Animal Status)
 *   domain: ethics/legal/political_economy
 *
 * SUMMARY:
 *   This story authors the welfare reading of the animal status kernel as a
 *   clean, epsilon-invariant constraint: a standing arrangement in which
 *   animals' interests are legally acknowledged as constraining human use —
 *   through welfare statutes, inspection regimes, and private assurance
 *   schemes — while exemption schedules and necessity clauses leave the bulk
 *   of actual use outside the floors' reach. The claim/metric split is
 *   deliberate: claimed_type is authored from structural facts (real
 *   coordination function plus asymmetric burden through the same
 *   architecture), the metrics from descriptive operation. Epsilon's referent
 *   is the existing welfare-governed use arrangement, assessed by this
 *   reading's own lights — not the abolitionist alternative this reading
 *   declines to endorse. KEY AGENTS (by structural relationship): -
 *   intensively_farmed_animals: primary target (powerless/trapped) — bears
 *   the arrangement's costs under exempted standard practices -
 *   laboratory_animals_under_necessity_waivers: secondary target
 *   (powerless/trapped) — protections waivable by the user institution's own
 *   determination - industrial_livestock_producers: primary beneficiary
 *   (institutional/arbitrage) — retains a bounded-use license and co-authors
 *   the exemptions - biomedical_research_establishments: secondary
 *   beneficiary (institutional/constrained) — operates under necessity-clause
 *   governance - meat_and_dairy_consumers: incidental beneficiary
 *   (organized/mobile) — secured supply at modest premium uptake -
 *   welfare_certification_scheme_operators: agenda-setter/beneficiary hybrid
 *   (organized/identity_locked) — administers private standards, collects the
 *   fee stream - welfare_statute_administrators: agenda setter
 *   (institutional/constrained) — authors statutes and exemption schedules
 *   under budget strain - animal_ethics_scholars: analytical observer — sees
 *   the full structure across readings
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__welfare_reading, 0.45).
domain_priors:suppression_score(animal_status__welfare_reading, 0.43).
domain_priors:theater_ratio(animal_status__welfare_reading, 0.56).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__welfare_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(animal_status__welfare_reading, suppression_requirement, 0.43).
narrative_ontology:constraint_metric(animal_status__welfare_reading, theater_ratio, 0.56).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__welfare_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(animal_status__welfare_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_status__welfare_reading, "Sentience-Constrained Use Arrangement (Welfare Reading of Animal Status)").
narrative_ontology:topic_domain(animal_status__welfare_reading, "ethics/legal/political_economy").

domain_priors:requires_active_enforcement(animal_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__welfare_reading, '133cf340-4190-45c4-b711-5fd316cdb7c1').
narrative_ontology:cs_kernel_codification('133cf340-4190-45c4-b711-5fd316cdb7c1', distributed).
narrative_ontology:cs_authority_grounding('133cf340-4190-45c4-b711-5fd316cdb7c1', distributed).
narrative_ontology:cs_reading_relation('133cf340-4190-45c4-b711-5fd316cdb7c1', animal_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('133cf340-4190-45c4-b711-5fd316cdb7c1', animal_status__property_reading, forecloses).
narrative_ontology:cs_axiom('133cf340-4190-45c4-b711-5fd316cdb7c1', foundational, sentience_grounds_constraining_interests).
narrative_ontology:cs_axiom_status(sentience_grounds_constraining_interests, holdable).
narrative_ontology:cs_axiom_grounding('133cf340-4190-45c4-b711-5fd316cdb7c1', sentience_grounds_constraining_interests, empirically_contingent).
narrative_ontology:cs_axiom('133cf340-4190-45c4-b711-5fd316cdb7c1', foundational, welfare_compliance_renders_use_legitimate).
narrative_ontology:cs_axiom_status(welfare_compliance_renders_use_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('133cf340-4190-45c4-b711-5fd316cdb7c1', welfare_compliance_renders_use_legitimate, instrumental).
narrative_ontology:cs_reference_frame('133cf340-4190-45c4-b711-5fd316cdb7c1', welfare_floored_instrumental_use).
narrative_ontology:cs_drift_state('133cf340-4190-45c4-b711-5fd316cdb7c1', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('133cf340-4190-45c4-b711-5fd316cdb7c1', '').
narrative_ontology:cs_kernel_id(animal_status__welfare_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, industrial_livestock_producers).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, biomedical_research_establishments).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, meat_and_dairy_consumers).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, welfare_certification_scheme_operators).
narrative_ontology:constraint_victim(animal_status__welfare_reading, intensively_farmed_animals).
narrative_ontology:constraint_victim(animal_status__welfare_reading, laboratory_animals_under_necessity_waivers).
narrative_ontology:constraint_vindicates(animal_status__welfare_reading, benthamite_sentience_doctrine).
narrative_ontology:constraint_vindicates(animal_status__welfare_reading, five_freedoms_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are raised and slaughtered inside production systems governed by welfare statutes whose floors cover transport conditions, stunning, and minimum housing, while the highest-volume practices are preserved by recognized-standard exemption schedules. An individual animal cannot refuse its conditions, relocate, or contest the standards that govern it; its interests reach the decision table only as represented by other seats.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, intensively_farmed_animals, payer,
    powerless, biographical, trapped, global).

% Are bred and used under research-welfare codes that nominally impose replacement, reduction, and refinement duties; a necessity determination issued by the user institution or its own committee can waive protections protocol by protocol. There is no exit: each animal is created for a protocol and disposed of within it.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, laboratory_animals_under_necessity_waivers, payer,
    powerless, immediate, trapped, global).

% Operate the largest-volume animal-use systems. Statutory floors bound their most visible practices while exemption schedules preserve the standard practices their margins depend on; they participate centrally in drafting the codes and exemption lists, fund the assurance schemes their customers recognize, and can shift production toward weaker-enforcement jurisdictions when rules tighten. Compliance costs land mainly at the edge of their operations.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, industrial_livestock_producers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(animal_status__welfare_reading, industrial_livestock_producers, agenda_setter).

% Run animal protocols under welfare codes whose protections yield to asserted scientific necessity. Relocation is possible but costly given funding relationships, ethics-committee dependencies, and established colonies, so they operate inside the framework rather than around it.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, biomedical_research_establishments, beneficiary,
    institutional, generational, constrained, global).

% Buy animal products whose availability and price the arrangement secures. Welfare labels offer an optional premium assurance channel that most purchases bypass. Plant-based substitutes are widely available and inexpensive, so leaving is easy in principle and lightly exercised in practice.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, meat_and_dairy_consumers, beneficiary,
    organized, immediate, mobile, global).

% Write and audit private welfare standards, license labels, and collect certification fees and donation streams tied to the framework's continuation. Their organizational purpose is constituted by the scheme they administer; abandoning it would dissolve the organization rather than relocate it.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, welfare_certification_scheme_operators, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(animal_status__welfare_reading, welfare_certification_scheme_operators, beneficiary).

% Draft welfare legislation, publish the exemption schedules and recognized-practice codes negotiated with industry bodies, and operate inspection regimes under constrained budgets. Their mandate and staffing depend on the framework's persistence; narrowing exemptions against producer opposition carries concentrated political cost in exchange for diffuse, unrepresented benefit.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, welfare_statute_administrators, agenda_setter,
    institutional, generational, constrained, national).

% Analyze the arrangement from outside its administration: comparing delivered protections against the interests the framework invokes, and mapping how alternative readings of animal status would redraw its boundaries. They collect nothing from the framework's operation and bear none of its costs.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animal_ethics_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status__welfare_reading, industrial_livestock_producers).
narrative_ontology:fixing_cost_class(animal_status__welfare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem: without common floors, producers who invest in welfare are undercut by those who do not; consumers cannot distinguish compliant from non-compliant products; researchers lack a shared justification protocol. Minimum standards, inspection, and labeling address the race to the bottom and the assurance problem at once.
% TRANSFER_FUNCTION: Moves welfare — freedom from pain, confinement, and early death — away from animals and into human hands as product availability, research capability, and price levels; moves assurance and legitimacy from administrators and certifiers to consumers; and places compliance costs on producers at the margin, with exemption schedules deciding how much of the animal-side transfer is actually stopped.
% ABSENT_VOICES: The animals themselves: every seat at every table where standards are written carries mixed interests except theirs, and no seat speaks with their interests alone — they appear as objects of administration, never as parties. Also absent: species outside the statutory frames (fish, cephalopods, most invertebrates) whose sentience evidence matured after the codes were fixed, and slaughterhouse and laboratory workers who absorb occupational costs the arrangement externalizes.
% DISAPPEARANCE_RATIONALE: If the welfare-constrained-use arrangement vanished overnight, animal use would not stop — it would reorganize around whichever successor frame filled the vacuum: effectively unconstrained use if no replacement constraint emerged, or rights-protected non-use under an abolitionist successor. Food systems, research pipelines, certification markets, and the administrative apparatus built on the framework would all restructure.
% FOUNDING_PROBLEM: Mid-century intensification of farming and expansion of laboratory use made animal suffering systemic, large-scale, and invisible to purchasers and voters; the arrangement was built to let useful animal use continue while bounding the suffering it causes and assuring the public that bounds exist.
% FOUNDING_PROBLEM_CORROBORATION: The Brambell Committee's 1965 report — a government inquiry convened after public outcry, before the benefiting industries shaped the legislative response — attests the founding problem; the peer-reviewed animal-welfare-science literature and successive official reviews attest that it remains live under exempted practices. None of these corroborating sources sits inside the commercial beneficiary set.
narrative_ontology:disappearance_verdict(animal_status__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__welfare_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__welfare_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_status__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__welfare_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__welfare_reading_tests).
:- end_tests(animal_status__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.45 is authored for the standing arrangement by this reading's own lights: enforceable floors (stunning requirements, transport density limits, inspection) deliver real protection at the margins, while recognized-practice exemptions leave the highest-volume practices outside those floors, so the gap between what the invoked interests would require and what the arrangement delivers is substantial but not total. Suppression 0.43 is a raw structural measure of the enforcement machinery's end-state intensity; it is unscaled — the engine scales only extractiveness. Theater_ratio 0.56: the audit, label, and assurance apparatus has grown past the binding-rule core it decorates, though stunning mandates and inspections remain functional. Accessibility_collapse 0.35: once the arrangement is understood, alternatives (plant substitution, cultivated protein, abolitionist reframing) remain visible and expanding rather than collapsing. Resistance 0.62: pressure arrives from both directions — producers resisting tightening and abolitionist constituencies rejecting the compromise — which is what a structure holding a contested middle position under fire looks like. Claimed type is tangled_rope on the structural facts: a genuine coordination function (race-to-the-bottom prevention, consumer assurance, research justification protocol) operating through the same architecture that exempts most use from its own floors, held up by active enforcement. Claim and metrics are authored independently; the engine computes per-seat classifications from the structural data. All three measurement series share one time grid (decade steps across T=0..60) so temporal reads sample every metric at every point.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the animals' position the arrangement is nearly pure imposition: they receive its costs, cannot exit, and are absent from its tables. From the producer position it is a purchased license: bounded rules that stabilize demand and exclude less scrupulous competitors, with jurisdiction-shopping available when rules bind. From the administrator position it is a governing achievement under budget strain. Two same-level contrasts sharpen the divergence: industrial producers and research establishments occupy the same institutional power tier but hold different exits (arbitrage versus constrained), so identical floors bind them unequally; and the two agenda-setter seats — statutory administrators and private certifiers — face different capture exposures, the certifiers structurally fused to the framework's continuation (institutional identity fusion: their mandate is the scheme, so exit equals self-dissolution). The engine computes these per-seat classifications from power, exit, and role data; nothing in the stakeholder surface pre-judges them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. The two animal groups are declared victims with trapped exit — they sit nearest the full-target end, so effective extraction reaches them close to full strength. Producers are declared beneficiaries with arbitrage exit — low directional value, further damped because relocation blunts whatever the floors attempt to recover. Consumers are beneficiaries with cheap mobile exit — near-symmetric, slightly subsidized by availability they rarely pay premiums for. Certifiers hold a dual position: they administer (agenda-setter) and collect fees (beneficiary), placing them well toward the beneficiary side despite performing real coordination work. Administrators are neither declared beneficiary nor victim; as constrained institutional agenda-setters they sit mildly toward the beneficiary side — the framework is their mandate. Spatial scope is global on the use-side seats, which raises verification difficulty and amplifies effective extraction onto the trapped targets; the administrator seat carries national scope. Suppression stays raw throughout — only extractiveness is scaled.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling intensified animal use with the interests that use frustrates — is still live, so this is not a resolved-mandatrophy case; the arrangement is doing its original job, imperfectly. The classification discipline matters in both directions here. Reading the structure as pure rope would launder the exemption architecture: the same statute that mandates stunning also schedules the majority of animals out from under the floors that would question their housing. Reading it as pure snare would erase the real floors: stunning and transport rules measurably reduce suffering at the margin, and an assurance market exists because some producers honor standards beyond what exemption requires. Tangled rope keeps both facts load-bearing. The trajectory to watch is theater: the measurement series shows the label and audit apparatus overtaking the binding-rule core while enforcement capacity thinned after mid-interval — if exempted volume keeps growing as enforcement decays, the structure drifts snare-ward and coordination becomes cover. The mismatch consumer can cross-check the live founding-problem status against the rising theater series.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint instantiates the welfare_reading of the animal_status kernel; how would the abolitionist_reading or the property_reading change the victim set and epsilon for the same standing subject matter?',
    'Generate the sibling stories over the fixed referent (existing welfare-governed use) and compare victim membership and epsilon: the property_reading removes animals from the victim set entirely (residual extraction approximates coordination cost only); the abolitionist_reading places all instrumentally-used animals in the victim set (epsilon approaches the rights-violation ceiling).',
    'Cross-reading comparison isolates what each permissibility claim does to measured extraction; divergence in computed type across the family is the corpus signal, not inconsistency to be averaged away.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: one of three readings of the animal_status kernel; sibling readings emit different victim sets and epsilons over the same referent.').

omega_variable(
    exemption_architecture_extraction,
    'Does the exemption architecture (recognized-standard and customary-practice carve-outs) carry the bulk of the arrangement''s measured extraction, or is extraction concentrated in the residual unprotected domains?',
    'Audit the share of animal-use volume proceeding under exempted practices versus enforceable-floor practices, and compare welfare-outcome incidence data across the two tracks.',
    'If exempted volume dominates, the headline protections are largely decorative, epsilon is understated, and the arrangement tilts snare-ward; if exempted volume is marginal, the floors do most of the work and epsilon is overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_architecture_extraction, empirical, 'Whether the exemption carve-outs or the enforceable floors dominate the arrangement''s actual operation.').

omega_variable(
    proxy_representation_fidelity,
    'Standards content is set by seats with mixed incentives — industry consultation, certifier fee dependence, administrator budget constraints — so does the negotiated content track the animals'' interests the constraint invokes?',
    'Compare outcomes where animal-interest representation was unusually direct (citizens'' assemblies, ballot initiatives on farm-animal confinement) against legislatively negotiated standards on comparable issues.',
    'Systematic divergence would establish agenda-setter capture: the coordination surface is real but its calibration serves the beneficiary seats, supporting the higher-extraction reading and snare-ward drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_representation_fidelity, empirical, 'Whether proxy-set standards track the interests of the parties who cannot appear.').

omega_variable(
    sufficiency_threshold_contest,
    'Is there any welfare-protection level at which instrumental use becomes genuinely legitimate under this reading''s own lights, or does the permissibility axiom (welfare_compliance_renders_use_legitimate) fail wherever net interest-violation remains?',
    'Specify the threshold condition within welfare ethics and test the best-performing certified systems against it, using welfare-outcome studies of highest-standard operations as empirical input.',
    'If no threshold survives scrutiny, this reading collapses toward abolitionism and epsilon for the standing arrangement approaches the rights-violation ceiling; if a threshold holds, the tangled-rope reading stands and reform rather than abolition is the remedy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sufficiency_threshold_contest, preference, 'The conceptual fault line with the abolitionist sibling: whether protection can ever be sufficient to legitimize use.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__welfare_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__welfare_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(anim_tr_t0, observed).
narrative_ontology:measurement(anim_tr_t10, animal_status__welfare_reading, theater_ratio, 10, 0.29).
narrative_ontology:measurement_basis(anim_tr_t10, observed).
narrative_ontology:measurement(anim_tr_t20, animal_status__welfare_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement_basis(anim_tr_t20, observed).
narrative_ontology:measurement(anim_tr_t30, animal_status__welfare_reading, theater_ratio, 30, 0.39).
narrative_ontology:measurement_basis(anim_tr_t30, observed).
narrative_ontology:measurement(anim_tr_t40, animal_status__welfare_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement_basis(anim_tr_t40, observed).
narrative_ontology:measurement(anim_tr_t50, animal_status__welfare_reading, theater_ratio, 50, 0.51).
narrative_ontology:measurement_basis(anim_tr_t50, observed).
narrative_ontology:measurement(anim_tr_t60, animal_status__welfare_reading, theater_ratio, 60, 0.56).
narrative_ontology:measurement_basis(anim_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__welfare_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(anim_be_t0, observed).
narrative_ontology:measurement(anim_be_t10, animal_status__welfare_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement_basis(anim_be_t10, observed).
narrative_ontology:measurement(anim_be_t20, animal_status__welfare_reading, base_extractiveness, 20, 0.34).
narrative_ontology:measurement_basis(anim_be_t20, observed).
narrative_ontology:measurement(anim_be_t30, animal_status__welfare_reading, base_extractiveness, 30, 0.37).
narrative_ontology:measurement_basis(anim_be_t30, observed).
narrative_ontology:measurement(anim_be_t40, animal_status__welfare_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement_basis(anim_be_t40, observed).
narrative_ontology:measurement(anim_be_t50, animal_status__welfare_reading, base_extractiveness, 50, 0.43).
narrative_ontology:measurement_basis(anim_be_t50, observed).
narrative_ontology:measurement(anim_be_t60, animal_status__welfare_reading, base_extractiveness, 60, 0.45).
narrative_ontology:measurement_basis(anim_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status__welfare_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement_basis(anim_su_t0, observed).
narrative_ontology:measurement(anim_su_t10, animal_status__welfare_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(anim_su_t10, observed).
narrative_ontology:measurement(anim_su_t20, animal_status__welfare_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement_basis(anim_su_t20, observed).
narrative_ontology:measurement(anim_su_t30, animal_status__welfare_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement_basis(anim_su_t30, observed).
narrative_ontology:measurement(anim_su_t40, animal_status__welfare_reading, suppression_requirement, 40, 0.51).
narrative_ontology:measurement_basis(anim_su_t40, observed).
narrative_ontology:measurement(anim_su_t50, animal_status__welfare_reading, suppression_requirement, 50, 0.47).
narrative_ontology:measurement_basis(anim_su_t50, observed).
narrative_ontology:measurement(anim_su_t60, animal_status__welfare_reading, suppression_requirement, 60, 0.43).
narrative_ontology:measurement_basis(anim_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__welfare_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(animal_status__welfare_reading, animal_status__abolitionist_reading).
narrative_ontology:affects_constraint(animal_status__welfare_reading, animal_status__property_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'animal status' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints over the same subject matter: this welfare reading (interests constrain but permit use; victims scoped to gratuitous and exemption-carried harm; epsilon ~0.45), the abolitionist reading (rights-bearing status precluding use; victim set spans all instrumental use; epsilon near ceiling), and the property reading (no independent standing; victim set empty; residual extraction approximates coordination cost). Lineage: the property frame is the historical legal baseline onto which welfare statutes were grafted, so this reading is structurally downstream of the property reading's infrastructure; the abolitionist reading contests this one from outside the framework it administers. Each family member links the other two; the epsilon spread across the family is the measurement the decomposition exists to take.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
