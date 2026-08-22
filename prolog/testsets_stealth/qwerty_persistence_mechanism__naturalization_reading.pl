% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__naturalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__naturalization_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: qwerty_persistence_mechanism__naturalization_reading
 *   human_readable: QWERTY Persistence as Merit-Selected Adequacy (Naturalization Reading)
 *   domain: economic_history/technology_studies
 *
 * SUMMARY:
 *   The QWERTY layout dominates text entry more than a century after its
 *   founding rationale — mechanical typebar jamming — ceased to exist. Under
 *   the naturalization reading, this persistence reflects genuine adequacy
 *   rather than enforcement or capture: the layout solves a real
 *   skill-interoperability problem, it imposes no fee or rent stream,
 *   alternatives such as Dvorak and Colemak have been legally and technically
 *   available in every mainstream operating system for decades, and they
 *   lapsed relative to QWERTY through ordinary competitive selection in which
 *   each adopter weighed real retraining costs against real (and, on this
 *   reading, unproven-as-large) gains. Switching costs are treated as the
 *   intrinsic price of acquiring any motor skill, not an artificially
 *   inflated moat. KEY AGENTS (by structural relationship): -
 *   operating_system_vendors: Administrator of the default
 *   (institutional/arbitrage) — ships QWERTY as the default because users
 *   already know it; collects no layout-specific revenue -
 *   qwerty_touch_typists: Primary beneficiary cohort (moderate/constrained) —
 *   one lifetime skill investment, decades of interoperability collected -
 *   keyboard_hardware_manufacturers: Incidental beneficiary
 *   (institutional/mobile) — scale economies from conformity, no enforcement
 *   role - typing_instruction_providers: Beneficiary (moderate/constrained) —
 *   curriculum stability tracks the standard -
 *   alternative_layout_communities: Marginalized advocate seat
 *   (moderate/mobile) — free to compete, absent from the rooms where defaults
 *   are chosen - technology_historians: Analytical observer
 *   (analytical/analytical) — reconstructs origins, debunks origin myths
 *   Family note: sibling stories
 *   (qwerty_persistence_mechanism__lock_in_reading,
 *   qwerty_persistence_mechanism__beneficiary_extraction_reading) author the
 *   same persistence fact under rival structures with named victims, active
 *   enforcement, and materially higher epsilon; this file's epsilon is low
 *   because this reading identifies no rent stream, no enforcement machinery,
 *   and no established inferiority. Each member of the family is a separate
 *   constraint with its own stable epsilon, linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - - operating_system_vendors: Administrator of the default (institutional/arbitrage) — selects the shipped layout, exposes alternatives as free settings options, collects no layout rent
 *   - - qwerty_touch_typists: Primary beneficiary cohort (moderate/constrained) — bears one skill-acquisition cost, collects network-wide interoperability
 *   - - keyboard_hardware_manufacturers: Incidental beneficiary (institutional/mobile) — conforms tooling to demand concentration; produces alternative layouts on request
 *   - - typing_instruction_providers: Beneficiary (moderate/constrained) — sells instruction keyed to the hiring-standard layout
 *   - - alternative_layout_communities: Marginalized advocate seat (moderate/mobile) — competes without barriers but holds no seat in procurement or education decisions
 *   - - technology_historians: Analytical observer (analytical/analytical) — attests the founding problem and its death from outside every beneficiary seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__naturalization_reading, 0.14).
domain_priors:suppression_score(qwerty_persistence_mechanism__naturalization_reading, 0.08).
domain_priors:theater_ratio(qwerty_persistence_mechanism__naturalization_reading, 0.16).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, extractiveness, 0.14).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 0.16).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__naturalization_reading, rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__naturalization_reading, "QWERTY Persistence as Merit-Selected Adequacy (Naturalization Reading)").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__naturalization_reading, "economic_history/technology_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__naturalization_reading, '91f8267a-b827-4b70-88d0-e9bbbb3b97df').
narrative_ontology:cs_kernel_codification('91f8267a-b827-4b70-88d0-e9bbbb3b97df', formalized).
narrative_ontology:cs_authority_grounding('91f8267a-b827-4b70-88d0-e9bbbb3b97df', practice).
narrative_ontology:cs_interpretation_layer_present('91f8267a-b827-4b70-88d0-e9bbbb3b97df').
narrative_ontology:cs_reading_relation('91f8267a-b827-4b70-88d0-e9bbbb3b97df', qwerty_persistence_mechanism__lock_in_reading, coexists_with).
narrative_ontology:cs_reading_relation('91f8267a-b827-4b70-88d0-e9bbbb3b97df', qwerty_persistence_mechanism__beneficiary_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('91f8267a-b827-4b70-88d0-e9bbbb3b97df', foundational, dvorak_advantage_empirically_unproven).
narrative_ontology:cs_axiom_status(dvorak_advantage_empirically_unproven, holdable).
narrative_ontology:cs_axiom_grounding('91f8267a-b827-4b70-88d0-e9bbbb3b97df', dvorak_advantage_empirically_unproven, empirically_contingent).
narrative_ontology:cs_axiom('91f8267a-b827-4b70-88d0-e9bbbb3b97df', foundational, retraining_cost_is_layout_generic).
narrative_ontology:cs_axiom_status(retraining_cost_is_layout_generic, holdable).
narrative_ontology:cs_axiom_grounding('91f8267a-b827-4b70-88d0-e9bbbb3b97df', retraining_cost_is_layout_generic, empirically_contingent).
narrative_ontology:cs_reference_frame('91f8267a-b827-4b70-88d0-e9bbbb3b97df', merit_selected_input_equilibrium).
narrative_ontology:cs_drift_state('91f8267a-b827-4b70-88d0-e9bbbb3b97df', post_fable_of_the_keys_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('91f8267a-b827-4b70-88d0-e9bbbb3b97df', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__naturalization_reading, qwerty_touch_typists).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__naturalization_reading, keyboard_hardware_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__naturalization_reading, typing_instruction_providers).
narrative_ontology:constraint_vindicates(qwerty_persistence_mechanism__naturalization_reading, market_selected_standards_can_be_efficient).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ship a default character layout on every consumer device. They select QWERTY as the default because their customers already know it, and expose alternative layouts as a settings option any user can enable at zero marginal cost. Changing the default would impose retraining on their entire installed base for a benefit their own product research cannot demonstrate; keeping it costs nothing beyond code they would ship anyway. They administer the arrangement's daily operation without collecting a layout-specific fee.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, operating_system_vendors, agenda_setter,
    institutional, generational, arbitrage, global).

% Invest roughly forty to eighty hours acquiring a layout whose value scales with how many other people share it: shared machines, temp pools, hiring tests, and borrowed laptops all assume it. Any individual can switch to an alternative in an afternoon of settings changes plus weeks of retraining, but doing so forfeits fluency on every keyboard they do not personally control. Most bear one layout acquisition in a lifetime and collect decades of interoperability from it.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, qwerty_touch_typists, beneficiary,
    moderate, biographical, constrained, global).

% Tool production lines and firmware around the dominant layout because volume concentrates there, gaining economies of scale and simplified inventory. Nothing stops them from producing alternative-layout boards — several do, on demand or as niche products — and the dominant layout's grip on their catalog reflects customer demand, not contractual obligation. They conform to the standard; they do not enforce it.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, keyboard_hardware_manufacturers, beneficiary,
    institutional, biographical, mobile, global).

% Sell courses, curricula, and certification keyed to the layout their students' future employers and examinations assume. Their materials amortize across cohorts only while the taught layout remains the hiring standard, so their commercial interest tracks the standard's persistence, though they hold no lever that maintains it beyond teaching it.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, typing_instruction_providers, beneficiary,
    moderate, biographical, constrained, national).

% Design, promote, and personally use alternative layouts (Dvorak, Colemak and descendants), publishing efficiency and comfort arguments and maintaining open-source mappings for every major platform. They face no legal or technical barrier — their layouts ship in mainstream operating systems — but they hold no seat in the procurement, education, or standards conversations where default choices are actually made, and their adoption arguments must overcome each audience's existing skill investment rather than a decision-maker's indifference.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, alternative_layout_communities, excluded,
    moderate, generational, mobile, global).

% Reconstruct the layout's origin in 1870s typebar mechanics, track the dissolution of its original rationale, and test surviving explanations for its persistence against patents, sales records, and the retraining literature. They hold no stake in which layout wins and publish the corrections — including debunked origin myths — that the other seats argue from.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, technology_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_mechanism__naturalization_reading, diffuse).
narrative_ontology:fixing_cost_class(qwerty_persistence_mechanism__naturalization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a skill-interoperability problem: a single shared layout lets any trained typist operate any keyboard, lets employers hire and reassign against a universal skill, and lets hardware makers build one compatible product line — coordination achieved through common knowledge rather than any enforcing body.
% TRANSFER_FUNCTION: Moves nothing continuously. Each new typist transfers tens of hours of training effort into a shared skill pool once; hardware makers absorb design-conformity costs; there is no fee, toll, or rent stream attached to the layout itself.
% ABSENT_VOICES: Alternative-layout designers and occupational-health researchers would object that efficiency and comfort gains are being left on the table; they are present in specialist literature and open-source communities but absent from the enterprise-procurement, education-ministry, and standards-committee rooms where defaults are actually chosen.
% DISAPPEARANCE_RATIONALE: Every shared keyboard, hiring test, and school curriculum assumes the layout; overnight disappearance would strand hundreds of millions of trained typists, force emergency retraining, and halt text entry on billions of devices until defaults were restored. The arrangement is load-bearing infrastructure, whatever one concludes about why it persists.
% FOUNDING_PROBLEM: Charles Sholes' 1870s typebar design problem: adjacent typebars on early mechanical typewriters jammed when struck in quick succession, and the letter arrangement was worked out — through the Remington manufacturing transition — to separate commonly paired letters mechanically and to let salesmen demonstrate by typing 'TYPEWRITER' rapidly from one row. Preventing jams and enabling sales demonstration, not maximizing typing speed, was the founding problem.
% FOUNDING_PROBLEM_CORROBORATION: Technology historians working from the Sholes patent record and Remington manufacturing archives — seats outside every beneficiary — corroborate the jam-separation origin; archival studies of the typewriter's development and museum collections independently attest that the layout's rationale died with mechanical typebars. No beneficiary seat's attestation is relied upon.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__naturalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__naturalization_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__naturalization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence_mechanism__naturalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__naturalization_reading, 0.14, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__naturalization_reading_tests).
:- end_tests(qwerty_persistence_mechanism__naturalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.14) because no seat collects a QWERTY-specific rent: the layout is given away embedded in devices, and the only costs it imposes are the contested residual of a possibly-suboptimal key arrangement. Suppression is very low (0.08) and is authored as a raw structural property — unscaled by power or scope; only extractiveness is scaled by the engine. No enforcement machinery exists to suppress anything: alternative layouts ship in every major OS behind a settings toggle. Theater ratio is low (0.16) because the standard requires no performance — it runs on habit and default inheritance — with a slight upward drift as public discussion turned mythological ('designed to slow typists down') after the operational debate faded. Accessibility collapse is moderate (0.50): individual exit is nearly free (an afternoon of settings plus weeks of retraining), but collective exit forfeits the network benefit that constitutes the standard's entire value, so alternatives remain accessible yet persistently unattractive. Resistance is low-moderate (0.25): periodic advocacy waves and niche professional adoption, but no sustained grievance infrastructure, because this reading identifies no injured class to organize. Claim and metrics are independent authored facts: claimed_type rope states this reading's structure (pure coordination, net beneficiaries, unsuppressed alternatives); the metrics describe observed operation. The temporal series run on one shared eight-point grid (1985-2026) with both tracked metrics authored at every point; no suppression_requirement series is authored because the enforcement picture is static-nil across the interval — that stability is carried by the scalar. Extractiveness declines gently across the interval as software remapping and cheap programmable keyboards lowered the real cost of individual exit.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the typist seat the arrangement is invisible infrastructure: near-zero experienced extraction, high collected benefit. From the alternative-layout advocate seat the same arrangement appears as an entrenched barrier — not because anyone blocks them, but because every audience they address must first destroy its own skill capital to hear the argument. From the OS-vendor seat the default is a customer-service decision with negative switching incentive. From the historian seat the whole structure is contingent: a mechanical-era artifact outliving its reason. The engine computes these per-seat classifications from the structural data; this story does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries map to real collection: typists collect interoperability proportional to everyone else's identical investment (damped d, near the beneficiary end); manufacturers collect scale economies without administering anything (damped d); instruction providers collect curriculum amortization (damped d). Operating system vendors administer the default but collect no QWERTY-specific fee — their relationship leans beneficiary-side (support-cost avoidance) without reaching full subsidy, and the derivation from their non-declared, non-victim position plus arbitrage-grade exit approximates this without an override. Alternative-layout communities are excluded from decision rooms but are not extracted from — they pay nothing to the arrangement they failed to displace. No victims are declared because this reading identifies none; that absence is a substantive structural claim of this reading, not an omission, and it is precisely what the sibling readings contest.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing adjacent-typebar jams in 1870s mechanical typewriters — has been dead for two generations, while the arrangement persists and the world would visibly rearrange without it. That mismatch (dead founding problem, world_rearranges verdict) correctly flags for review, and the classification apparatus prevents the two opposite mislabels it invites. Reading it as a snare would require a rent stream and coerced victims; none are identifiable — no seat pays a QWERTY toll and no seat enforces the layout. Reading it as a piton would require theatrical maintenance and an administrator who bears fix-costs exceeding its own burden; observed theater is low, administration is minimal (defaults passively follow the user base), and the fix cost falls almost entirely on users, not the administrator. What the structural data show instead is function migration: the arrangement's live justification moved from jam-avoidance to skill-coordination, and the coordination function is performed, not performed-at. A rope whose founding warrant expired but whose replacement warrant is genuine is not a zombie; the low theater ratio and the absence of any capturing seat are the discriminating evidence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dvorak_advantage_magnitude,
    'Does the Dvorak layout confer a real, material advantage in speed, accuracy, or comfort over QWERTY?',
    'Preregistered, longitudinally controlled retraining trials with QWERTY-control groups, blinded for prior exposure, converging on independent ergonomic and productivity endpoints.',
    'A large, replicated advantage would contradict this reading''s foundational axiom and shift the persistence explanation toward the lock-in sibling reading; a negligible or unreplicable advantage secures the adequacy claim and this story''s low epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dvorak_advantage_magnitude, empirical, 'Size and replicability of the Dvorak-over-QWERTY advantage.').

omega_variable(
    incumbent_maintenance_behavior,
    'Did incumbents (manufacturers, large employers, standards committees) actively maintain QWERTY or suppress alternatives, or merely follow user demand?',
    'Archival study of manufacturer product-line decisions, employer keyboard policies (including the wartime Dvorak experiments and their abandonment), and standards-committee minutes, scored for gatekeeping versus demand-following.',
    'Documented active suppression would import victims and enforcement into the structure and move this story toward the beneficiary-extraction sibling; its absence leaves the no-systematic-beneficiary declaration intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_maintenance_behavior, empirical, 'Whether incumbent conduct constitutes active maintenance or passive conformity.').

omega_variable(
    adequacy_vs_entrenchment_boundary,
    'Is ''genuine adequacy'' separable from network-effect entrenchment — can fair competitive selection be distinguished from path dependence when the winning option is also an adequate one?',
    'Counterfactual analysis: would a layout with QWERTY''s properties win from a cold start in replacement markets (soft keyboards, new device categories), and would any first-mover layout persist equally regardless of merit?',
    'If adequacy and entrenchment are conceptually inseparable, this reading collapses into the lock-in sibling with a different epsilon and victim structure; if separable, the naturalization claim stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adequacy_vs_entrenchment_boundary, conceptual, 'Whether the adequacy explanation is conceptually distinct from entrenchment.').

omega_variable(
    kernel_reading_commitment,
    'This story instantiates the naturalization_reading of kernel qwerty_persistence_mechanism; would the lock_in_reading or beneficiary_extraction_reading of the same persistence fact compile as a structurally different constraint?',
    'Compare the compiled sibling stories: victim sets, enforcement flags, and epsilon should differ by reading while the referent arrangement (QWERTY''s persistence) stays fixed across all three.',
    'Cross-reading divergence is expected and diagnostic; identical structural outputs across readings would indicate the kernel was not actually contested and the reading distinction is vacuous.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Indexical commitment: this file is one reading of a three-reading kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__naturalization_reading, 1985, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1985, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 1985, 0.1).
narrative_ontology:measurement_basis(qwer_tr_t1985, observed).
narrative_ontology:measurement(qwer_tr_t1990, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 1990, 0.11).
narrative_ontology:measurement_basis(qwer_tr_t1990, observed).
narrative_ontology:measurement(qwer_tr_t1996, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 1996, 0.12).
narrative_ontology:measurement_basis(qwer_tr_t1996, observed).
narrative_ontology:measurement(qwer_tr_t2002, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 2002, 0.13).
narrative_ontology:measurement_basis(qwer_tr_t2002, observed).
narrative_ontology:measurement(qwer_tr_t2008, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 2008, 0.13).
narrative_ontology:measurement_basis(qwer_tr_t2008, observed).
narrative_ontology:measurement(qwer_tr_t2014, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 2014, 0.14).
narrative_ontology:measurement_basis(qwer_tr_t2014, observed).
narrative_ontology:measurement(qwer_tr_t2020, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement_basis(qwer_tr_t2020, observed).
narrative_ontology:measurement(qwer_tr_t2026, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 2026, 0.16).
narrative_ontology:measurement_basis(qwer_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1985, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 1985, 0.2).
narrative_ontology:measurement_basis(qwer_be_t1985, observed).
narrative_ontology:measurement(qwer_be_t1990, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 1990, 0.19).
narrative_ontology:measurement_basis(qwer_be_t1990, observed).
narrative_ontology:measurement(qwer_be_t1996, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 1996, 0.18).
narrative_ontology:measurement_basis(qwer_be_t1996, observed).
narrative_ontology:measurement(qwer_be_t2002, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 2002, 0.17).
narrative_ontology:measurement_basis(qwer_be_t2002, observed).
narrative_ontology:measurement(qwer_be_t2008, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 2008, 0.16).
narrative_ontology:measurement_basis(qwer_be_t2008, observed).
narrative_ontology:measurement(qwer_be_t2014, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 2014, 0.15).
narrative_ontology:measurement_basis(qwer_be_t2014, observed).
narrative_ontology:measurement(qwer_be_t2020, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 2020, 0.15).
narrative_ontology:measurement_basis(qwer_be_t2020, observed).
narrative_ontology:measurement(qwer_be_t2026, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 2026, 0.14).
narrative_ontology:measurement_basis(qwer_be_t2026, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(qwerty_persistence_mechanism__naturalization_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__naturalization_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism__lock_in_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism__beneficiary_extraction_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'QWERTY persistence.' The natural-language phrase covers three structurally distinct claims that share one referent arrangement but differ in epsilon, beneficiary structure, and enforcement profile: (1) naturalization_reading (this file) — persistence through adequacy, epsilon ~0.14, no victims, no enforcement; (2) lock_in_reading — persistence through coordination failure despite inferiority, intermediate epsilon, diffuse user-side costs; (3) beneficiary_extraction_reading — persistence through active incumbent maintenance, high epsilon, named victims, active enforcement. The upstream empirical question (magnitude of the Dvorak advantage) feeds all three: this reading cites its favorable resolution as evidence, so edges run from this story to both siblings. Per the epsilon-invariance principle these are three constraints, not one constraint viewed from three angles; each file carries its own claimed_type and metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
