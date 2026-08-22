% ============================================================================
% CONSTRAINT STORY: us_constitution_text__living_constitutionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__living_constitutionalist_reading, []).

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
 *   constraint_id: us_constitution_text__living_constitutionalist_reading
 *   human_readable: Living Constitutionalist Reading of Constitutional Meaning
 *   domain: constitutional_law/legal_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the living-constitutionalist reading of the
 *   shared kernel 'us_constitution_text': the claim that constitutional
 *   meaning legitimately evolves with society, and that interpretation must
 *   adapt founding-era principles to contemporary circumstances rather than
 *   being bound to the ratifiers' specific expected applications. This
 *   reading empowers judges to treat the text's abstract terms (equal
 *   protection, due process, cruel and unusual punishment) as inviting
 *   generational reinterpretation, drawing legitimacy from post-ratification
 *   practice, evolving social consensus, and changed factual circumstances.
 *   It is generated here as a single, ε-invariant constraint: it does not
 *   describe or average over the originalist or positivist readings, which
 *   are separate constraint stories with their own ε and stakeholder
 *   structures, linked via the kernel and cs_structure.reading_relations.
 *
 * KEY AGENTS:
 *   - reform_oriented_judiciary: Primary agenda-setter (institutional/analytical) — sets and applies the adaptive interpretive doctrine
 *   - rights_claimants_in_changed_social_contexts: Primary beneficiary (moderate/constrained) — secures recognition unavailable under fixed founding-era meaning
 *   - social_movements_seeking_constitutional_recognition: Organized beneficiary and excluded party — wins in court but is bypassed in the amendment process
 *   - fixed_meaning_democratic_constraint_advocates: Primary payer (organized/constrained) — experiences the loss of a stable, amendment-only constraint
 *   - legislatures_bypassed_by_judicial_updating: Institutional payer — displaced policymaking authority
 *   - constitutional_law_scholars: Analytical observer — documents and evaluates the interpretive method itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__living_constitutionalist_reading, 0.38).
domain_priors:suppression_score(us_constitution_text__living_constitutionalist_reading, 0.22).
domain_priors:theater_ratio(us_constitution_text__living_constitutionalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__living_constitutionalist_reading, rope).
narrative_ontology:human_readable(us_constitution_text__living_constitutionalist_reading, "Living Constitutionalist Reading of Constitutional Meaning").
narrative_ontology:topic_domain(us_constitution_text__living_constitutionalist_reading, "constitutional_law/legal_philosophy").

domain_priors:requires_active_enforcement(us_constitution_text__living_constitutionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__living_constitutionalist_reading, '7834b1b1-a74e-4289-a8fc-c1ec81967d32').
narrative_ontology:cs_kernel_codification('7834b1b1-a74e-4289-a8fc-c1ec81967d32', fixed_text).
narrative_ontology:cs_authority_grounding('7834b1b1-a74e-4289-a8fc-c1ec81967d32', lineage).
narrative_ontology:cs_interpretation_layer_present('7834b1b1-a74e-4289-a8fc-c1ec81967d32').
narrative_ontology:cs_reading_relation('7834b1b1-a74e-4289-a8fc-c1ec81967d32', us_constitution_text__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('7834b1b1-a74e-4289-a8fc-c1ec81967d32', us_constitution_text__positivist_reading, influences).
narrative_ontology:cs_axiom('7834b1b1-a74e-4289-a8fc-c1ec81967d32', foundational, constitutional_principles_require_generational_reapplication).
narrative_ontology:cs_axiom_status(constitutional_principles_require_generational_reapplication, holdable).
narrative_ontology:cs_axiom_grounding('7834b1b1-a74e-4289-a8fc-c1ec81967d32', constitutional_principles_require_generational_reapplication, instrumental).
narrative_ontology:cs_axiom('7834b1b1-a74e-4289-a8fc-c1ec81967d32', foundational, post_ratification_practice_and_social_consensus_carry_interpretive_authority).
narrative_ontology:cs_axiom_status(post_ratification_practice_and_social_consensus_carry_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('7834b1b1-a74e-4289-a8fc-c1ec81967d32', post_ratification_practice_and_social_consensus_carry_interpretive_authority, conventional).
narrative_ontology:cs_reference_frame('7834b1b1-a74e-4289-a8fc-c1ec81967d32', framers_general_principles_applied_generationally).
narrative_ontology:cs_drift_state('7834b1b1-a74e-4289-a8fc-c1ec81967d32', contemporary_rights_expansion_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7834b1b1-a74e-4289-a8fc-c1ec81967d32', '').
narrative_ontology:cs_kernel_id(us_constitution_text__living_constitutionalist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__living_constitutionalist_reading, rights_claimants_in_changed_social_contexts).
narrative_ontology:constraint_beneficiary(us_constitution_text__living_constitutionalist_reading, reform_oriented_judiciary).
narrative_ontology:constraint_beneficiary(us_constitution_text__living_constitutionalist_reading, social_movements_seeking_constitutional_recognition).
narrative_ontology:constraint_victim(us_constitution_text__living_constitutionalist_reading, fixed_meaning_democratic_constraint_advocates).
narrative_ontology:constraint_victim(us_constitution_text__living_constitutionalist_reading, legislatures_bypassed_by_judicial_updating).
narrative_ontology:constraint_vindicates(us_constitution_text__living_constitutionalist_reading, constitution_as_evolving_charter_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals and groups (e.g., same-sex couples seeking marriage recognition, women seeking reproductive autonomy) whose claims were not contemplated or were foreclosed under founding-era understanding. They rely on courts reading constitutional principles at a level of generality that accommodates contemporary circumstances rather than the specific applications ratifiers had in mind. Without an adaptive reading, their only recourse is amendment or legislation, both far harder to secure.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, rights_claimants_in_changed_social_contexts, beneficiary,
    moderate, biographical, constrained, national).

% Judges, particularly at the appellate and Supreme Court level, who adopt and apply the living-constitutionalist interpretive method. They set the doctrine that translates abstract text into binding contemporary rules, drawing on evolving standards of decency, post-ratification practice, and changed social facts. Their authority to do this is itself contested, but their institutional position lets them exercise it regardless.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, reform_oriented_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Organized advocacy movements (civil rights, marriage equality, disability rights) that litigate strategically to secure adaptive readings. They benefit enormously when courts update doctrine, but remain excluded from the formal amendment process that would settle their claims more durably and democratically.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, social_movements_seeking_constitutional_recognition, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_text__living_constitutionalist_reading, social_movements_seeking_constitutional_recognition, excluded).

% Citizens, scholars, and political actors who value the Constitution as a fixed constraint on majoritarian and judicial power precisely because its meaning does not move with elite or judicial preference. Under a living reading, the promise that constitutional meaning is stable and only changeable through Article V amendment is treated as unenforceable; they experience each adaptive ruling as unilateral rewriting of terms they did not consent to and cannot easily reverse.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, fixed_meaning_democratic_constraint_advocates, payer,
    organized, generational, constrained, national).

% State and federal legislative bodies whose policymaking authority over contested social questions is displaced when courts resolve them as constitutional matters via adaptive interpretation. Legislatures could in principle also update the law, but living constitutionalism moves those questions into judicial rather than democratic channels, foreclosing legislative compromise and experimentation.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, legislatures_bypassed_by_judicial_updating, payer,
    institutional, generational, constrained, national).

% Academics and commentators who study and debate interpretive methodology itself. They document how living constitutionalism has expanded and contracted doctrine over time and assess its legitimacy relative to originalist and positivist alternatives, without a direct stake in any particular outcome.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_text__living_constitutionalist_reading, diffuse).
narrative_ontology:fixing_cost_class(us_constitution_text__living_constitutionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for a two-centuries-old founding text to remain applicable and legitimate as social conditions, technology, and moral understanding change, without requiring supermajority amendment for every adjustment.
% TRANSFER_FUNCTION: Moves interpretive authority over contested social questions from the amendment process and ordinary legislatures to the judiciary, and moves recognized rights from groups excluded under founding-era applications to groups claiming inclusion under updated readings of the same abstract text.
% ABSENT_VOICES: The ratifying generation and, more concretely, contemporary legislative majorities who might resolve these same questions democratically are structurally sidelined once a matter is recast as a matter of adaptive constitutional principle rather than ordinary policy; they are not in the courtroom when the doctrine is set.
% DISAPPEARANCE_RATIONALE: If living constitutionalism disappeared as an interpretive method, courts would revert to originalist or positivist modes; many rights currently secured by adaptive rulings (rights not explicitly contemplated at ratification) would become vulnerable to reversal or would require legislative or amendment action to preserve, and a large body of precedent built on evolving-standards reasoning would be structurally destabilized.
% FOUNDING_PROBLEM: The Constitution's text is brief, general, and was ratified for an agrarian 18th-century society; without some interpretive method for extending its principles, courts would face either paralysis on questions the framers never addressed or a rigid application of framers' specific expectations that later generations may find morally intolerable (e.g., segregation, sex discrimination).
% FOUNDING_PROBLEM_CORROBORATION: Living-constitutionalist judges and scholars attest the founding problem (textual generality meeting changed circumstances) is permanently live. Originalist scholars and some legislators, from outside the beneficiary set, attest that the problem is better characterized as adequately solved by the Article V amendment process, and that living constitutionalism substitutes judicial preference for that process rather than genuinely solving an interpretive gap; independent legal historians note the debate itself predates any settled resolution.
narrative_ontology:disappearance_verdict(us_constitution_text__living_constitutionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__living_constitutionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__living_constitutionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_text__living_constitutionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__living_constitutionalist_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__living_constitutionalist_reading_tests).
:- end_tests(us_constitution_text__living_constitutionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored as moderate (0.38 at interval end) because the living-constitutionalist reading does transfer real decisional authority away from legislatures and away from the amendment process's consent requirements, but this transfer occurs alongside a genuine coordination function (keeping a brief, general 18th-century text workable for a changed society) — this is not a pure extraction story. Suppression is authored low (0.22): the reading does not forcibly suppress alternative interpretive methods; originalist and positivist readings remain fully articulable, taught, litigated, and periodically ascendant on the same courts. Accessibility collapse is moderate-low (0.35): once a case is decided under adaptive doctrine, reversing it requires either overturning precedent or amendment, which is difficult but not impossible, as periodic doctrinal reversals demonstrate. Resistance is authored high (0.62) because the reading provokes sustained, organized political and scholarly contest — it is one of the most actively disputed methodological questions in American law, and this contestation is a fact about the reading's actual operation, independent of the claimed type.
 *
 * PERSPECTIVAL GAP:
 *   From the reform-oriented judiciary's seat, this operates as workable coordination: a functioning translation mechanism keeping the Constitution alive across radically different social eras. From the seat of fixed-meaning advocates and bypassed legislatures, the same mechanism appears as a device by which unelected judges substitute their own contemporary moral judgments for both the ratifiers' terms and current majoritarian preference, extracting decisional authority through a doctrine that has no textual limiting principle. The engine's per-seat computation should surface this divergence structurally rather than have it asserted here.
 *
 * DIRECTIONALITY LOGIC:
 *   Rights claimants and social movements are declared beneficiaries because the adaptive method is precisely what allows their claims to succeed where a fixed-meaning approach would foreclose them — their directionality sits toward the beneficiary end. Fixed-meaning advocates and bypassed legislatures are declared payers because the predictability and democratic-channel guarantee they value is exactly what the adaptive method displaces; their directionality sits toward the target end, tempered by their genuinely constrained (not trapped) exit — they retain political, electoral, and judicial-appointment avenues, just heavily attenuated ones operating on long generational timelines.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a brief, general text needing application to unanticipated circumstances) remains genuinely live for genuinely unanticipated questions (e.g., digital privacy, novel biotechnologies), which argues against treating this as pure mandatrophy. But for many high-salience applications, the 'contemporary circumstances' invoked are themselves the product of decades of social movement organizing that could plausibly pursue amendment or legislation instead — the persistent choice of the judicial-adaptive channel over the harder democratic channels is what keeps founding_problem_status contested rather than settled as live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptive_reading_reading_identity,
    'Is this constraint properly one reading among three structurally distinct readings of a single contested kernel (the constitutional text), or does treating ''the Constitution''s meaning'' as a single object with three interpretive overlays obscure that these are effectively three different constraints wearing one label?',
    'This is resolved by construction per the ε-invariance principle: each reading is authored as its own constraint story with its own ε, beneficiaries, victims, and metrics, linked via cs_structure.reading_relations rather than folded into one story with an interpretive-method parameter.',
    'Confirms that this story''s ε (0.38) is not comparable to or averageable with the originalist or positivist readings'' ε values; each is a distinct measurement of a distinct constraint sharing only the underlying text.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adaptive_reading_reading_identity, conceptual, 'Whether the three constitutional interpretive readings constitute genuinely separate constraints (resolved: yes, per decomposition rule).').

omega_variable(
    judicial_authority_vs_democratic_bypass,
    'Does the living-constitutionalist method genuinely solve an interpretive necessity (text cannot mechanically apply itself to unanticipated circumstances) or does it primarily function to relocate contested social-policy questions from slower democratic channels to faster judicial ones?',
    'Comparative analysis of cases decided under adaptive doctrine: distinguish cases involving genuinely unanticipated technological/factual circumstances (where no democratic channel existed to consult) from cases resolving long-standing, actively-legislated moral disputes (where democratic channels existed and were bypassed).',
    'If the former dominates, the coordination function is substantial and the rope/tangled_rope boundary favors rope; if the latter dominates, this reading functions closer to a tangled rope or snare against the bypassed legislative and fixed-meaning constituencies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_authority_vs_democratic_bypass, empirical, 'Whether adaptive interpretation solves genuine textual gaps or substitutes for legislative/amendment action.').

omega_variable(
    beneficiary_stability_of_adaptive_gains,
    'Are rights secured through living-constitutionalist reasoning durable, or are they only as stable as the next shift in judicial interpretive philosophy — meaning the ''benefit'' to rights claimants may be a temporary artifact of the same doctrinal flexibility that could later be used to remove them?',
    'Track reversal rates of rights established primarily through evolving-standards or adaptive reasoning versus rights established through amendment or clear textual grounding, over multi-decade windows.',
    'If adaptively-secured rights are reversed at meaningfully higher rates than amendment-secured rights, the beneficiary designation for rights claimants should be understood as conditional and time-bounded rather than a stable structural position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_stability_of_adaptive_gains, empirical, 'Whether adaptive-doctrine-based rights gains are as durable as they appear.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__living_constitutionalist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_text__living_constitutionalist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_text__living_constitutionalist_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_text__living_constitutionalist_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_text__living_constitutionalist_reading, theater_ratio, 30, 0.21).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_text__living_constitutionalist_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(us_c_tr_t50, us_constitution_text__living_constitutionalist_reading, theater_ratio, 50, 0.26).
narrative_ontology:measurement(us_c_tr_t60, us_constitution_text__living_constitutionalist_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(us_c_be_t10, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 10, 0.26).
narrative_ontology:measurement(us_c_be_t20, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 20, 0.31).
narrative_ontology:measurement(us_c_be_t30, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 30, 0.34).
narrative_ontology:measurement(us_c_be_t40, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 40, 0.36).
narrative_ontology:measurement(us_c_be_t50, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 50, 0.37).
narrative_ontology:measurement(us_c_be_t60, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 60, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(us_c_su_t10, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 10, 0.16).
narrative_ontology:measurement(us_c_su_t20, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 20, 0.18).
narrative_ontology:measurement(us_c_su_t30, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 30, 0.19).
narrative_ontology:measurement(us_c_su_t40, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 40, 0.2).
narrative_ontology:measurement(us_c_su_t50, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 50, 0.21).
narrative_ontology:measurement(us_c_su_t60, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 60, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__living_constitutionalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_text__living_constitutionalist_reading, us_constitution_text__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__living_constitutionalist_reading, us_constitution_text__positivist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint stories decomposed from the single natural-language label 'constitutional interpretation' / 'the meaning of the Constitution.' The originalist_reading authors low extraction toward rights claimants relying on non-textually-anticipated applications and treats fixed-meaning advocates as beneficiaries rather than victims; the positivist_reading brackets moral/historical content entirely and authors its own distinct ε around procedural validity questions. All three share the same underlying kernel (the ratified constitutional text) but instantiate structurally distinct constraints with different beneficiary/victim sets and different ε values, per the ε-invariance principle. They are linked here via affects_constraints; each carries this same note.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
