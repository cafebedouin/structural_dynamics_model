% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__substitutionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__substitutionist_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: divine_marriage_command__substitutionist_reading
 *   human_readable: Divine Marriage Command (Substitutionist Reading): Monogamy as Superseding Revelation
 *   domain: religious/political_theology
 *
 * SUMMARY:
 *   The substitutionist reading of the divine marriage command holds that
 *   monogamy is the genuine doctrinally mandated form of marriage, revealed
 *   through the Manifesto as a superseding revelation that displaces the
 *   prior allowance of polygamy. This reading is one of three structurally
 *   distinct interpretations of the same kernel (the divine revelation
 *   governing marriage doctrine). The substitutionist reading frames the
 *   Manifesto as a doctrinal correction rooted in new divine understanding,
 *   not as institutional capitulation to federal legal pressure. This framing
 *   serves institutional consolidation by presenting the reinterpretation as
 *   theologically authentic rather than coerced. The tension is between the
 *   institutional legitimacy claim (this is what God now requires) and the
 *   historical fact (this is what the government forced us to adopt). The
 *   substitutionist reading dissolves that tension by treating the historical
 *   pressure as coincidental to the theological truth.
 *
 * KEY AGENTS:
 *   - institutional_church_hierarchy: Sets the substitutionist doctrine, controls excommunication, and benefits from the consolidation it produces
 *   - polygamist_fundamentalists: Held plural marriage as doctrinally mandated; now classified as apostate; identity-locked exit
 *   - women_in_plural_marriages: Embedded in relationships the institution now declares illegitimate; constrained exit options
 *   - institutional_doctrinal_dissenters: Scholars who read the prior revelation as binding; face professional suppression
 *   - federal_coercive_authority: Applied the external pressure that structured the institutional choice; absent from the theological framing
 *   - mainstream_religious_observers: Track the theological coherence and institutional legitimacy of the reinterpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, 0.68).
domain_priors:suppression_score(divine_marriage_command__substitutionist_reading, 0.71).
domain_priors:theater_ratio(divine_marriage_command__substitutionist_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__substitutionist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__substitutionist_reading, "Divine Marriage Command (Substitutionist Reading): Monogamy as Superseding Revelation").
narrative_ontology:topic_domain(divine_marriage_command__substitutionist_reading, "religious/political_theology").

domain_priors:requires_active_enforcement(divine_marriage_command__substitutionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__substitutionist_reading, 'c381c7fa-1c7c-4fca-abba-a98aae8191ca').
narrative_ontology:cs_kernel_codification('c381c7fa-1c7c-4fca-abba-a98aae8191ca', formalized).
narrative_ontology:cs_authority_grounding('c381c7fa-1c7c-4fca-abba-a98aae8191ca', extraction).
narrative_ontology:cs_interpretation_layer_present('c381c7fa-1c7c-4fca-abba-a98aae8191ca').
narrative_ontology:cs_reading_relation('c381c7fa-1c7c-4fca-abba-a98aae8191ca', divine_marriage_command__continuationist_reading, forecloses).
narrative_ontology:cs_reading_relation('c381c7fa-1c7c-4fca-abba-a98aae8191ca', divine_marriage_command__coercion_visibility_reading, influences).
narrative_ontology:cs_axiom('c381c7fa-1c7c-4fca-abba-a98aae8191ca', foundational, revelation_supersession_doctrine).
narrative_ontology:cs_axiom_status(revelation_supersession_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('c381c7fa-1c7c-4fca-abba-a98aae8191ca', revelation_supersession_doctrine, deontological).
narrative_ontology:cs_axiom('c381c7fa-1c7c-4fca-abba-a98aae8191ca', foundational, monogamy_divinely_mandated).
narrative_ontology:cs_axiom_status(monogamy_divinely_mandated, holdable).
narrative_ontology:cs_axiom_grounding('c381c7fa-1c7c-4fca-abba-a98aae8191ca', monogamy_divinely_mandated, empirically_contingent).
narrative_ontology:cs_reference_frame('c381c7fa-1c7c-4fca-abba-a98aae8191ca', prior_polygamy_revelation).
narrative_ontology:cs_drift_state('c381c7fa-1c7c-4fca-abba-a98aae8191ca', post_manifesto_consolidation, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('c381c7fa-1c7c-4fca-abba-a98aae8191ca', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__substitutionist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, institutional_church_hierarchy).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, polygamist_fundamentalists).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, women_in_plural_marriages).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__substitutionist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(divine_marriage_command__substitutionist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__substitutionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__substitutionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 to 0.68 over 30 years as the substitutionist reading hardens into institutional doctrine. Early extractiveness is moderate because the reinterpretation is still contested and fundamentalists retain some institutional voice. As the reading consolidates, extractiveness rises because institutional enforcement (excommunication, credibility revocation, publication gatekeeping) intensifies against dissent. The plateau after year 30 reflects the reading's achievement of near-total institutional control; further extraction would require expanding enforcement beyond the already-contained dissent. Theater_ratio falls from 0.72 to 0.52 as the constraint shifts from performative revelation (the initial dramatic announcement of the Manifesto) to functional doctrinal reality (enforced marriage norm). The theater is not eliminated — the substitutionist framing itself is theatrical (presenting coerced capitulation as revelation) — but the ratio declines because institutional machinery for enforcement becomes more salient than the rhetorical cover. Suppression_requirement rises from 0.58 to 0.71 as active enforcement becomes necessary to hold the constraint against fundamentalist resistance and doctrinal dissent. The rise reflects intensifying institutional action: excommunications, exclusions from authority, public delegitimization of the prior reading. The measurement series run on a single shared time grid (shared across all three metrics at every time point) so the temporal divergence is real, not an artifact of misaligned sampling.
 *
 * PERSPECTIVAL GAP:
 *   The institutional hierarchy and the polygamist fundamentalists should compute differently under the engine's per-seat classification. From the hierarchy's seat, the constraint is coordination (doctrine that clarifies marriage practice and unifies institutional teaching) with only incidental extraction (the dissenters' suppression is necessary discipline, not the point). From the fundamentalist seat, the constraint is pure extraction (their prior doctrinal status is revoked, their marriages are declared apostasy, their community is dismantled) with a coordination cover story (the claim that monogamy is newly revealed). The hierarchy's directionality should be near 0.0 (beneficiary), the fundamentalists' near 1.0 (target). Women in plural marriages sit in the middle: some experience liberation from coercive marriage norms, others experience abandonment of their relational reality by the institution that originally mandated the marriages. Dissenters sit closer to target (d ~0.7) because their professional standing is suppressed by the consolidated reading. The engine's per-seat computation surfaces this perspectival asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional hierarchy is the structural beneficiary (d near 0.0 → low χ or subsidy): they set the constraint, control its enforcement, and gain institutional consolidation from it. Polygamist fundamentalists are the structural targets (d near 1.0 → high χ): they lose doctrinal standing, community membership, and relational respectability through the reinterpretation. Their identity is locked to the prior revelation — leaving the faith entirely is their only non-suppressed exit. Women in plural marriages face constrained exit (economic dependence, institutional monopoly on respectability) and partial victimhood (some liberation from norms, some abandonment). Doctrinal dissenters face suppression of their scholarly voice and professional standing, placing them at d ~0.65-0.75. Federal coercive authority is excluded from the theological framing entirely — their role is structurally denied rather than incorporated. The hierarchy's beneficiary status is not offset by the victims' oppression in the metrics because suppression is a raw structural property unscaled by power or scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The substitutionist reading carries a potential mandatrophy: the constraint was ostensibly founded to answer a coordination problem (clarifying marriage doctrine under pressure) but persists primarily as institutional enforcement of a reinterpretation against dissent. The founding problem (federal coercion forcing institutional choice on marriage doctrine) is technically resolved if framed in the substitutionist terms — the resolution was the adoption of the new doctrine, and the constraint enforces what was chosen. However, the mandatrophy becomes visible when examining the theater_ratio trajectory: the constraint began highly theatrical (the dramatic Manifesto announcement) and moved toward functional enforcement (excommunications, publication gatekeeping). This is the inverse of a genuine rope, which would begin functional and only accumulate theater if it degraded. The theater decline here suggests the constraint is solving what the theater initially claimed to solve (doctrinal clarification) while intensifying extraction against residual dissent. The classification as tangled_rope (rather than snare) is justified by the real coordination function (doctrine does clarify marriage practice for mainstream institutional members) coexisting with asymmetric extraction (fundamentalists and dissenters bear the cost without benefit). The mandatrophy is contained by this hybrid character: the constraint is not pure extraction (the doctrine really does coordinate practice) but it is not pure coordination (the extraction against dissenters is necessary for the coordination to hold).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_vs_coercion_ambiguity,
    'Is the Manifesto a genuinely new divine revelation, or is it institutional capitulation to federal coercion dressed in revelatory language?',
    'This ambiguity is irreducible in principle — no empirical data can distinguish an authentic revelation from a well-executed institutional narrative that claims revelation. The resolution mechanism is theological authority: the institutional hierarchy claims revelatory status; external analysts and fundamentalist dissenters dispute it. Resolution would require either a metacommunity consensus on revelation criteria (not available cross-denominationally) or the dissolution of institutional authority claims (foreclosing the substitutionist reading).',
    'If the Manifesto is genuine revelation, the substitutionist reading is correct and mandatrophy does not apply. If it is institutional capitulation, mandatrophy is active: the constraint persists as enforcement of a coerced reinterpretation against those who held the prior doctrine as binding. The institutional hierarchy benefits from the ambiguity by occupying both readings simultaneously (claiming revelation while benefiting from coercion-accommodation).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revelation_vs_coercion_ambiguity, conceptual, 'The irreducible ambiguity between genuine revelation and institutional narrative.').

omega_variable(
    suppression_internalization_in_identity_lock,
    'For polygamist fundamentalists with identity_locked exit: is their suppression structural (enforced excommunication and community ostracism) or has it been internalized such that they experience the monogamy doctrine as morally true even after institutional pressure is removed?',
    'Post-exit trajectory analysis: if fundamentalists who leave the institutional faith retain belief in monogamy doctrine, suppression has become internalized. If they revert to polygamy doctrine after gaining exit freedom, suppression was primarily structural.',
    'If suppression is structural, the constraint''s effective suppression_requirement is what the metrics report (0.71 at interval end). If suppression has been internalized, the constraint''s effective suppression is higher — the target carries the suppression after exit. Internalization would indicate more successful institutional capture of the victim''s self-concept.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_in_identity_lock, empirical, 'The mechanism and durability of suppression in identity-locked agents.').

omega_variable(
    women_in_plural_marriages_beneficiary_ambiguity,
    'Do women in plural marriages experience the substitutionist reinterpretation as liberation (relief from coercive marriage structures) or as abandonment (institutional rejection of their relational reality)?',
    'Testimony and post-reinterpretation cohort analysis: women who report relief identify as partial beneficiaries; women who report abandonment are victims. The distribution between these cohorts determines whether the constraint''s victim class is uniform or stratified.',
    'If most women in plural marriages experience relief, the beneficiary class should expand to include them (partial beneficiary role), and extraction is concentrated on fundamentalists and dissenters. If most experience abandonment, the victim class expands, and extraction is broader. The ambiguity reflects the institutional use of feminist rhetoric to justify the reinterpretation while abandoning women whose marriages the institution originally mandated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(women_in_plural_marriages_beneficiary_ambiguity, empirical, 'The heterogeneity of women''s experience under the reinterpretation.').

omega_variable(
    doctrinal_dissenter_coalition_power,
    'Could doctrinal dissenters form a coalition with polygamist fundamentalists to challenge the substitutionist reading, or does their institutional positioning (scholars vs. lay practitioners) prevent coalition formation?',
    'Historical analysis of dissent patterns and coalition attempts. If dissenters and fundamentalists have coordinated resistance, coalition power exists. If they remain isolated, coalitional power is absent.',
    'Coalition power would lower suppression_requirement (the constraint would face more organized resistance) and could shift the classification from tangled_rope toward snare (pure extraction under pressure). Absent coalitional power, dissenters are individualized through professional suppression and fundamentalists are communally isolated through excommunication.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_dissenter_coalition_power, empirical, 'The possibility of unified resistance across dissenting constituencies.').

omega_variable(
    continuationist_reading_foreclosure_claim,
    'Does the substitutionist reading logically foreclose the continuationist reading (making both impossible to hold within the same institutional framework), or do they coexist as competing live interpretations?',
    'Logical analysis: if the substitutionist claim (monogamy is now divinely commanded) contradicts the continuationist claim (polygamy remains doctrinally valid) such that no institutional framework could hold both, they foreclose. If the continuationist reading can frame the Manifesto as suspension rather than rescission, the readings coexist within different institutional communities.',
    'If substitutionist forecloses continuationist, the kernel has only two live readings (substitutionist and coercion_visibility). If they coexist, three readings remain live, and fundamentalist continuationists retain a defensible theological position. The impact on mandatrophy is significant: coexistence allows the possibility of future reversal (the institution could revert to continuationism), while foreclosure makes the substitutionist reading more permanent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(continuationist_reading_foreclosure_claim, conceptual, 'Whether the substitutionist reading logically rules out the continuationist alternative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__substitutionist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_marriage_command__substitutionist_reading, theater_ratio, 0, 0.72).
narrative_ontology:measurement(divi_tr_t5, divine_marriage_command__substitutionist_reading, theater_ratio, 5, 0.68).
narrative_ontology:measurement(divi_tr_t10, divine_marriage_command__substitutionist_reading, theater_ratio, 10, 0.64).
narrative_ontology:measurement(divi_tr_t15, divine_marriage_command__substitutionist_reading, theater_ratio, 15, 0.6).
narrative_ontology:measurement(divi_tr_t20, divine_marriage_command__substitutionist_reading, theater_ratio, 20, 0.56).
narrative_ontology:measurement(divi_tr_t25, divine_marriage_command__substitutionist_reading, theater_ratio, 25, 0.54).
narrative_ontology:measurement(divi_tr_t30, divine_marriage_command__substitutionist_reading, theater_ratio, 30, 0.52).
narrative_ontology:measurement(divi_tr_t40, divine_marriage_command__substitutionist_reading, theater_ratio, 40, 0.52).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_marriage_command__substitutionist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(divi_be_t5, divine_marriage_command__substitutionist_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(divi_be_t10, divine_marriage_command__substitutionist_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(divi_be_t15, divine_marriage_command__substitutionist_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(divi_be_t20, divine_marriage_command__substitutionist_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(divi_be_t25, divine_marriage_command__substitutionist_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(divi_be_t30, divine_marriage_command__substitutionist_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(divi_be_t40, divine_marriage_command__substitutionist_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_marriage_command__substitutionist_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(divi_su_t5, divine_marriage_command__substitutionist_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(divi_su_t10, divine_marriage_command__substitutionist_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(divi_su_t15, divine_marriage_command__substitutionist_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(divi_su_t20, divine_marriage_command__substitutionist_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(divi_su_t25, divine_marriage_command__substitutionist_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(divi_su_t30, divine_marriage_command__substitutionist_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(divi_su_t40, divine_marriage_command__substitutionist_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__substitutionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_marriage_command__substitutionist_reading, 0.12).
narrative_ontology:affects_constraint(divine_marriage_command__substitutionist_reading, divine_marriage_command__continuationist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__substitutionist_reading, divine_marriage_command__coercion_visibility_reading).

% DUAL FORMULATION NOTE:
% The substitutionist, continuationist, and coercion_visibility readings are three structurally distinct constraints grounded in the same contested kernel (divine marriage command). They differ in ε (extractiveness), beneficiary/victim structure, and the theological framework used to justify the post-Manifesto monogamy requirement. The substitutionist reading treats the Manifesto as new revelation; the continuationist treats it as prudential suspension; the coercion_visibility reading acknowledges institutional response to federal pressure. Each reading has its own story file. They are linked via affects_constraints because the institutional adoption of the substitutionist reading forecloses or constrains the other readings' institutional legitimacy. The decomposition follows the ε-invariance principle: the same kernel-text (the divine revelation on marriage) yields three distinct constraints depending on which reading is instantiated, and each reading carries a different extractiveness profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(divine_marriage_command__substitutionist_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
